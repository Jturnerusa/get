#![deny(clippy::use_self)]

#[proc_macro_derive(Get, attributes(get))]
pub fn get(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let parsed_input = syn::parse_macro_input!(input as syn::DeriveInput);
    get::expand(&parsed_input, false).unwrap().into()
}

#[proc_macro_derive(GetCopy, attributes(get))]
pub fn get_copy(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let parsed_input = syn::parse_macro_input!(input as syn::DeriveInput);
    get::expand(&parsed_input, true).unwrap().into()
}

mod get {

    use if_chain::if_chain;
    use proc_macro2::{Span, TokenStream};
    use quote::{format_ident, quote, ToTokens};
    use syn::{
        parse::Parser, punctuated::Punctuated, Attribute, Data, DeriveInput, Expr, Field, Fields,
        Ident, Index, Lit, Member, Meta, MetaNameValue, Token, Type,
    };

    #[derive(Default)]
    struct GetAttribute {
        method: Option<String>,
    }

    #[derive(Debug, Clone)]
    enum GetNameValue {
        Method(String),
    }

    pub fn expand(
        input: &DeriveInput,
        is_copy: bool,
    ) -> Result<TokenStream, Box<dyn std::error::Error>> {
        let Data::Struct(target) = &input.data else {
            return Err("expected struct as derive input".into())
        };
        let getters = match &target.fields {
            Fields::Unnamed(fields) => expand_for_tuple_struct(fields.unnamed.iter(), is_copy)?,
            Fields::Named(fields) => expand_for_struct(fields.named.iter(), is_copy)?,
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
        is_copy: bool,
    ) -> Result<TokenStream, Box<dyn std::error::Error>> {
        let mut tokens = TokenStream::new();
        for field in fields {
            let method_name = match field.attrs.iter().find_map(|attr| {
                attr.path()
                    .is_ident("get")
                    .then(|| GetAttribute::try_from(attr.clone()))
            }) {
                Some(Ok(a)) if a.method.is_some() => {
                    format_ident!("{}", a.method.unwrap().as_str())
                }
                Some(Err(e)) => return Err(e),
                _ => field.ident.as_ref().cloned().unwrap(),
            };
            let getter = expand_getter(
                field,
                Member::Named(field.ident.as_ref().unwrap().clone()),
                &method_name,
                is_copy,
            );
            getter.to_tokens(&mut tokens);
        }
        Ok(tokens)
    }

    fn expand_for_tuple_struct<'a>(
        fields: impl Iterator<Item = &'a Field>,
        is_copy: bool,
    ) -> Result<TokenStream, Box<dyn std::error::Error>> {
        let mut tokens = TokenStream::new();
        for (i, field) in fields.enumerate() {
            let method_name = match field.attrs.iter().find_map(|attr| {
                attr.path()
                    .is_ident("get")
                    .then(|| GetAttribute::try_from(attr.clone()))
            }) {
                Some(Ok(get_attr)) if get_attr.method.is_some() => {
                    format_ident!("{}", get_attr.method.unwrap().as_str())
                }
                Some(Err(e)) => return Err(e),
                _ => return Err(r#"tuple fields are required to have an attribute"#.into()),
            };
            let getter = expand_getter(
                field,
                Member::Unnamed(Index {
                    index: i as u32,
                    span: Span::call_site(),
                }),
                &method_name,
                is_copy,
            );
            getter.to_tokens(&mut tokens);
        }
        Ok(tokens)
    }

    fn expand_getter(
        field: &Field,
        field_name: Member,
        method_name: &Ident,
        is_copy: bool,
    ) -> TokenStream {
        let field_type = &field.ty;
        let field_lifetime = match &field.ty {
            Type::Reference(type_ref) => Some(&type_ref.lifetime),
            _ => None,
        };
        let method_args = if is_copy {
            quote! { ( self ) }
        } else {
            quote! {  ( & #field_lifetime self )  }
        };
        let method_type = if is_copy {
            quote! { #field_type }
        } else {
            quote! { & #field_type }
        };
        let method_body = if is_copy {
            quote! { { self . #field_name } }
        } else {
            quote! { { & self . #field_name } }
        };
        quote! {
            pub fn
            #method_name
            #method_args
            -> #method_type
            #method_body
        }
    }

    // This method might seem overly complicated but it will make it easy to add new fields to the
    // GetAttribute struct in the future!

    impl TryFrom<Attribute> for GetAttribute {
        type Error = Box<dyn std::error::Error>;
        fn try_from(attr: Attribute) -> Result<Self, Self::Error> {
            if_chain! {
                if attr.path().is_ident("get");
                if let Meta::List(list) = &attr.meta;
                then {
                    Ok(Punctuated::<MetaNameValue, Token![,]>::parse_terminated
                        .parse(list.tokens.clone().into())?
                        .into_iter()
                        .map(|n| n.try_into())
                        .collect::<Result<Vec<GetNameValue>, _>>()
                        .map(|v| match v.len() {
                            1..=3 => Ok(v),
                            _ => Err("expected at least 1 name value pair in attribute")
                        })??
                        .into_iter()
                        .fold(Self::default(), |_, n| match n {
                            GetNameValue::Method(s) => Self {
                                method: Some(s),
                            },
                        }))
                } else {
                    Err("failed to parse attribute".into())
                }
            }
        }
    }

    impl TryFrom<MetaNameValue> for GetNameValue {
        type Error = Box<dyn std::error::Error>;
        fn try_from(meta: MetaNameValue) -> Result<Self, Self::Error> {
            if_chain! {
                if let Some(name) = meta.path.get_ident().map(|ident| ident.to_string());
                if let Expr::Lit(expr) = &meta.value;
                if let Lit::Str(s) = &expr.lit;
                if let "method" = name.as_str();
                then {
                    Ok(Self::Method(s.value()))
                } else {
                    Err("invalid name value pair in attribute".into())
                }
            }
        }
    }
}
