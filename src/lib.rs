#![deny(clippy::use_self)]

//! Another getter derive macro.
//!
//! # Examples:
//! #### Regular struct:
//! ```
//! use get::Get;
//!
//! #[derive(Get)]
//! struct Crab {
//!     name: String,
//!     age: u64,
//!     // Sometimes we may not want to provide access to a field at all.
//!     #[get(hide)]
//!     secrets: String
//! }
//!
//! fn crab() {
//!     let ferris = Crab::new("ferris", 10);
//!     assert!(matches!(    
//!         ferris.name().as_str(),
//!         "ferris"
//!     ));
//!     assert_eq!(*ferris.age(), 10);
//! }
//!
//!# impl Crab {
//!#     pub fn new(name: &str, age: u64) -> Self {
//!#         Self { name: name.to_string(), age, secrets: "crab secrets".to_string() }
//!#     }
//!# }
//!```
//! #### Tuple struct:
//! ```
//! use get::Get;
//!
//! #[derive(Get)]
//! struct Crab (
//!     #[get(method = "name")] String,
//!     #[get(method = "age")] u64,
//! );
//!
//! fn crab() {
//!     let ferris = Crab::new("ferris", 10);
//!     assert!(matches!(    
//!         ferris.name().as_str(),
//!         "ferris"
//!     ));
//!     assert_eq!(*ferris.age(), 10);
//! }
//!
//!# impl Crab {
//!#     pub fn new(name: &str, age: u64) -> Self {
//!#         Self ( name.to_string(), age )
//!#     }
//!# }
//!```
//!#### Getters on Copy types:
//!```
//! use get::Get;
//!
//! #[derive(Clone, Copy, Get)]
//! struct NonZeroUInt<T>(
//!     #[get(method = "inner", kind = "copy")] T
//! );
//!
//! fn non_zero_uint() {
//!     let i = NonZeroUInt::new(1).unwrap();
//!     // The getter method takes "self" by value.
//!     assert_eq!(i.inner(), 1);
//!     // Since NonZeroUint::<u32> is Copy, the value is still accessible.
//!     assert_eq!(i.inner() + 1, 2);
//! }
//!
//!# impl NonZeroUInt<u32> {
//!#     fn new(i: u32) -> Option<Self> {
//!#         (i != 0).then(|| Self(i))
//!#     }
//!#  }
//!```
//! # Attributes
//! Attributes are expected to contain a comma separated list of name value pairs or idents.
//!
//! Examples of supported attributes:
//! * `#[get(method = "getter")]`
//! * `#[get(hide)]`
//!
//! All supported name value pairs:
//! * `method` (this sets the name of the getter method)
//! * `kind` (this sets the kind of getter, the supported getter kinds are listed below)
//!
//! All supported idents are:
//! * `hide` (this will disable getters for a specific field)
//!
//! All supported getter kinds are (ref is the default):
//! * `ref`
//! * `copy`
//! * `deref`
//! 
//! # Todos
//! * detect and return error for fields with conflicting attributes
//! * improve error output, include span information if possible

#[proc_macro_derive(Get, attributes(get))]
pub fn get(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let parsed_input = syn::parse_macro_input!(input as syn::DeriveInput);
    get::expand(&parsed_input).unwrap().into()
}

mod get {
    use core::option::Option::{self, None};

    use proc_macro2::{Span, TokenStream};
    use quote::{ToTokens, format_ident, quote};
    use syn::{
        Attribute, Data, DeriveInput, Expr, Field, Fields, Ident, Index, Lit, Member,
        MetaNameValue, Token, Type, parse::Parser, punctuated::Punctuated,
    };

    enum GetAttribute {
        NameValueList(GetNameValueList),
        IdentList(Vec<GetIdent>),
    }

    #[derive(Default)]
    struct GetNameValueList(Vec<GetNameValue>);

    #[derive(Debug, Clone)]
    enum GetNameValue {
        Method(String),
        Kind(String),
    }

    #[derive(Debug, Clone)]
    enum GetIdent {
        Hide,
    }

    #[derive(Debug, Clone, Copy)]
    pub enum GetterKind {
        Ref,
        Move,
        Deref,
    }

    impl GetNameValueList {
        fn method(&self) -> Option<String> {
            self.0.iter().find_map(|x| match x {
                GetNameValue::Method(name) => Some(name.clone()),
                _ => None,
            })
        }

        fn kind(&self) -> Option<GetterKind> {
            self.0.iter().find_map(|x| match x {
                GetNameValue::Kind(kind) => Some(match kind.as_str() {
                    "ref" => GetterKind::Ref,
                    "move" => GetterKind::Move,
                    "deref" => GetterKind::Deref,
                    _ => return None,
                }),
                _ => None,
            })
        }
    }

    pub fn expand(input: &DeriveInput) -> Result<TokenStream, Box<dyn std::error::Error>> {
        let Data::Struct(target) = &input.data else {
            return Err("expected struct as derive input".into());
        };
        let getters = match &target.fields {
            Fields::Unnamed(fields) => expand_for_tuple_struct(fields.unnamed.iter())?,
            Fields::Named(fields) => expand_for_struct(fields.named.iter())?,
            _ => return Err("can not generate getters on a unit struct".into()),
        };
        let (impl_generics, ty_generics, where_clause) = &input.generics.split_for_impl();
        let struct_name = &input.ident;
        Ok(quote! {
            #[automatically_derived]
            impl #impl_generics  #struct_name #ty_generics  #where_clause {
                #getters
            }
        })
    }

    fn expand_for_struct<'a>(
        fields: impl Iterator<Item = &'a Field>,
    ) -> Result<TokenStream, Box<dyn std::error::Error>> {
        let mut tokens = TokenStream::new();
        for field in fields {
            match field.attrs.iter().find_map(|attr| {
                attr.path()
                    .is_ident("get")
                    .then(|| GetAttribute::try_from(attr.clone()))
            }) {
                Some(Ok(GetAttribute::IdentList(list)))
                    if list.iter().any(|i| matches!(i, GetIdent::Hide)) =>
                {
                    continue;
                }
                Some(Ok(GetAttribute::NameValueList(list))) => expand_getter(
                    field,
                    &Member::Named(field.ident.as_ref().cloned().unwrap()),
                    &list
                        .method()
                        .map(|name| format_ident! {"{name}"})
                        .unwrap_or(field.ident.as_ref().cloned().unwrap()),
                    list.kind().unwrap_or(GetterKind::Ref),
                ),
                Some(Err(e)) => return Err(e),
                _ => expand_getter(
                    field,
                    &Member::Named(field.ident.as_ref().cloned().unwrap()),
                    field.ident.as_ref().unwrap(),
                    GetterKind::Ref,
                ),
            }
            .to_tokens(&mut tokens);
        }
        Ok(tokens)
    }

    fn expand_for_tuple_struct<'a>(
        fields: impl Iterator<Item = &'a Field>,
    ) -> Result<TokenStream, Box<dyn std::error::Error>> {
        let mut tokens = TokenStream::new();
        for (i, field) in fields.enumerate() {
            match field.attrs.iter().find_map(|attr| {
                attr.path()
                    .is_ident("get")
                    .then(|| GetAttribute::try_from(attr.clone()))
            }) {
                Some(Ok(GetAttribute::IdentList(list)))
                    if list.iter().any(|i| matches!(i, GetIdent::Hide)) =>
                {
                    continue;
                }
                Some(Ok(GetAttribute::NameValueList(list))) if list.method().is_some() => {
                    expand_getter(
                        field,
                        &Member::Unnamed(Index {
                            index: i.try_into().unwrap(),
                            span: Span::call_site(),
                        }),
                        &list.method().map(|s| format_ident!("{s}")).unwrap(),
                        list.kind().unwrap_or(GetterKind::Ref),
                    )
                }
                Some(Err(e)) => return Err(e),
                _ => return Err("expected attribute on tuple struct field".into()),
            }
            .to_tokens(&mut tokens)
        }
        Ok(tokens)
    }

    fn expand_getter(
        field: &Field,
        field_name: &Member,
        method_name: &Ident,
        kind: GetterKind,
    ) -> TokenStream {
        let field_type = &field.ty;

        let field_lifetime = match &field.ty {
            Type::Reference(type_ref) => Some(&type_ref.lifetime),
            _ => None,
        };

        let method_args = if let GetterKind::Move = kind {
            quote! { ( self ) }
        } else {
            quote! {  ( & #field_lifetime self )  }
        };

        let method_type = match kind {
            GetterKind::Ref => quote! {
                 & #field_type
            },
            GetterKind::Move => quote! {
                 #field_type
            },
            GetterKind::Deref => quote! {
                & < #field_type as :: std :: ops :: Deref > :: Target
            },
        };

        let method_body = match kind {
            GetterKind::Ref => quote! {
                { & self . #field_name }
            },
            GetterKind::Move => quote! {
               { self . #field_name }
            },
            GetterKind::Deref => quote! {
                { < #field_type as std :: ops :: Deref > :: deref ( &self . #field_name ) }
            },
        };

        quote! {
            pub fn
            #method_name
            #method_args
            -> #method_type
            #method_body
        }
    }

    impl TryFrom<Attribute> for GetAttribute {
        type Error = Box<dyn std::error::Error>;
        fn try_from(attr: Attribute) -> Result<Self, Self::Error> {
            let meta_list = attr
                .meta
                .require_list()
                .map_err(|_| "failed to parse attribute")?;
            let name_value_list = Punctuated::<MetaNameValue, Token![,]>::parse_terminated
                .parse(meta_list.tokens.clone().into());
            let ident_list = Punctuated::<Ident, Token![,]>::parse_terminated
                .parse(meta_list.tokens.clone().into());
            Ok(match (name_value_list, ident_list) {
                (Ok(list), _) => Self::NameValueList(GetNameValueList::try_from(list)?),
                (_, Ok(list)) => {
                    Self::IdentList(list.into_iter().map(GetIdent::try_from).collect::<Result<
                        Vec<GetIdent>,
                        _,
                    >>(
                    )?)
                }
                _ => return Err("failed to parse attribute".into()),
            })
        }
    }

    #[allow(clippy::needless_update)]
    impl TryFrom<Punctuated<MetaNameValue, Token![,]>> for GetNameValueList {
        type Error = Box<dyn std::error::Error>;
        fn try_from(punct: Punctuated<MetaNameValue, Token![,]>) -> Result<Self, Self::Error> {
            Ok(Self(
                punct
                    .into_iter()
                    .map(GetNameValue::try_from)
                    .collect::<Result<Vec<GetNameValue>, _>>()
                    .map(|v| {
                        if !v.is_empty() {
                            Ok::<Vec<GetNameValue>, Box<dyn std::error::Error>>(v)
                        } else {
                            Err("expected at least 1 name value pair in attribute".into())
                        }
                    })??,
            ))
        }
    }

    impl TryFrom<Ident> for GetIdent {
        type Error = Box<dyn std::error::Error>;
        fn try_from(i: Ident) -> Result<Self, Self::Error> {
            match i.to_string().as_str() {
                "hide" => Ok(Self::Hide),
                _ => Err(r#"expected the following ident in meta list: "hide""#.into()),
            }
        }
    }

    impl TryFrom<MetaNameValue> for GetNameValue {
        type Error = Box<dyn std::error::Error>;
        fn try_from(meta: MetaNameValue) -> Result<Self, Self::Error> {
            if let Some(name) = meta.path.get_ident().map(|ident| ident.to_string())
                && let Expr::Lit(expr) = &meta.value
                && let Lit::Str(s) = &expr.lit
            {
                match name.as_str() {
                    "method" => Ok(Self::Method(s.value())),
                    "kind" => Ok(Self::Kind(s.value())),
                    _ => Err(format!("invalid value in attribute: {}", name.as_str()).into()),
                }
            } else {
                Err("invalid name value pair in attribute".into())
            }
        }
    }
}
