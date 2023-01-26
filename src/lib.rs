use proc_macro::TokenStream;
use syn::token::Struct;

#[proc_macro]
pub fn variadic(input: TokenStream) -> TokenStream {
    let _parsed: Struct = syn::parse(input).unwrap();
    quote::quote!(
        struct T {}
    )
    .into()
}

mod parse {
    use std::collections::VecDeque;

    use proc_macro2::{TokenStream, TokenTree};
    use quote::quote;
    use syn::{parse::Parse, punctuated::Punctuated, GenericParam, Ident, Token, TypeParam};

    #[derive(Clone)]
    enum VariadicGenericParam {
        Param(GenericParam),
        VariadicParam(TypeParam),
    }

    impl Parse for VariadicGenericParam {
        fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
            if input.peek(Token![...]) {
                _ = input.parse::<Token![...]>()?;
                Ok(VariadicGenericParam::VariadicParam(input.parse()?))
            } else {
                Ok(VariadicGenericParam::Param(input.parse()?))
            }
        }
    }

    #[derive(Clone)]
    pub struct VariadicStruct {
        _struct: Token![struct],
        name: Ident,
        _start_generic: Token![<],
        params: Punctuated<VariadicGenericParam, Token![,]>,
        _end_generic: Token![>],
        rest: TokenStream,
    }

    impl Parse for VariadicStruct {
        fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
            Ok(VariadicStruct {
                _struct: input.parse()?,
                name: input.parse()?,
                _start_generic: input.parse()?,
                params: Punctuated::parse_separated_nonempty(input)?,
                _end_generic: input.parse()?,
                rest: input.parse()?,
            })
        }
    }

    struct ParseUntil<T: Parse> {
        before: TokenStream,
        parsed: Option<T>,
        after: TokenStream,
    }

    impl<T: Parse> Parse for ParseUntil<T> {
        fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
            let mut before: Vec<TokenTree> = Default::default();
            loop {
                if input.is_empty() {
                    return Ok(ParseUntil {
                        before: TokenStream::from_iter(before),
                        parsed: None,
                        after: TokenStream::new(),
                    });
                }
                let parsed: syn::Result<T> = input.fork().parse();
                match parsed {
                    Err(_) => before.push(input.parse()?),
                    Ok(_) => {
                        return Ok(ParseUntil {
                            before: TokenStream::from_iter(before),
                            parsed: Some(input.parse()?),
                            after: input.parse()?,
                        })
                    }
                }
            }
        }
    }

    fn new_ident(id: Ident, num: i32) -> Ident {
        let new_name = id.to_string() + "__" + &num.to_string();
        Ident::new(&new_name, id.span())
    }

    pub fn expand(input: TokenStream) -> syn::Result<TokenStream> {
        use VariadicGenericParam::*;
        let VariadicStruct {
            name, params, rest, ..
        } = syn::parse2(input)?;
        let variadic_params: Vec<_> = params
            .clone()
            .into_iter()
            .filter_map(|n| match n {
                VariadicParam(param) => Some(param),
                Param(_) => None,
            })
            .collect();
        let final_params: Vec<_> = params
            .into_iter()
            .flat_map(|v| match v {
                VariadicParam(param) => [GenericParam::Type(TypeParam {
                    ident: new_ident(param.ident, 0),
                    ..param
                })],
                Param(p) => [p],
            })
            .collect();

        let struct_decl = quote! {struct #name <#(#final_params),*>};
        let mut tokens = rest;
        let mut expanded_tokens: VecDeque<TokenTree> = VecDeque::from_iter(struct_decl);
        while !tokens.is_empty() {
            let parsed: ParseUntil<Token![...]> = syn::parse2(tokens)?;
            expanded_tokens.extend(parsed.before.into_iter());
            tokens = parsed.after;
        }
        Ok(TokenStream::from_iter(expanded_tokens))
    }

    #[test]
    fn test_parse_variadic_struct() {
        let t: VariadicStruct =
            syn::parse2(quote::quote! {struct blah <T, U..., V...> { a: i32, b: i32}})
                .map_err(|e| {
                    println!("error {:?} {:?}", e, e.span());
                    e
                })
                .unwrap();
        println!("{:?}", t.rest);
        let VariadicGenericParam::VariadicParam(p1) = &t.params.into_iter().collect::<Vec<_>>()[1] else {
            panic!()
        };
        println!("{:?}", p1.ident);
    }

    #[test]
    fn test_expand() {
        let e = expand(quote::quote! {
            struct Toast <...T> {
                a: (...T)
            };
        });
        println!("{}", e.unwrap());
    }
}
