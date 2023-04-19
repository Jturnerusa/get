#![deny(clippy::use_self)]

#[proc_macro_derive(Get, attributes(get))]
pub fn get(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let parsed_input = syn::parse_macro_input!(input as syn::DeriveInput);
    get::expand(&parsed_input, false).unwrap().into()
}

mod get {

    use if_chain::if_chain;
    use proc_macro2::{Span, TokenStream};
    use quote::{format_ident, quote, ToTokens};
    use syn::{
        parse::Parser, punctuated::Punctuated, Attribute, Data, DeriveInput, Expr, Field, Fields,
        Index, Lit, Member, Meta, MetaNameValue, Token, Type,
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
            let default_method_name = field.ident.as_ref().cloned().unwrap().to_string();
            let method_name = field
                .attrs
                .iter()
                .find(|attr| attr.path().is_ident("get"))
                .cloned()
                .map_or(Ok(default_method_name.clone()), |attr| {
                    GetAttribute::try_from(attr)
                        .map(|get_attr| get_attr.method.unwrap_or(default_method_name.clone()))
                })?;
            let getter = expand_getter(
                field,
                Member::Named(field.ident.as_ref().unwrap().clone()),
                method_name.as_str(),
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
            let attr: GetAttribute = field
                .attrs
                .iter()
                .find(|a| a.path().is_ident("get"))
                .cloned()
                .ok_or("tuple fields are required to have an attribute")?
                .try_into()?;
            let method_name = attr
                .method
                .ok_or("tuple field attributes must specify the method name")?;
            let getter = expand_getter(
                field,
                Member::Unnamed(Index {
                    index: i as u32,
                    span: Span::call_site(),
                }),
                method_name.as_str(),
                is_copy,
            );
            getter.to_tokens(&mut tokens);
        }
        Ok(tokens)
    }

    fn expand_getter(
        field: &Field,
        field_name: Member,
        method_name: &str,
        is_copy: bool,
    ) -> TokenStream {
        let method_name = format_ident!("{method_name}");
        let field_type = &field.ty;
        let field_lifetime = match &field.ty {
            Type::Reference(type_ref) => Some(&type_ref.lifetime),
            _ => None,
        };
        let reference = (!is_copy).then(|| quote! { & });
        quote! {
            pub fn
            #method_name
            ( #reference #field_lifetime self )
            -> #reference #field_type {
                #reference self.#field_name
            }
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
