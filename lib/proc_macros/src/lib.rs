use std::{cell::RefCell, rc::Rc};

use convert_case::Casing;
use proc_macro::TokenStream;
use proc_macro2::{Punct, Spacing, TokenTree};
use quote::{format_ident, quote};
use syn::{parse_macro_input, Data, DeriveInput, Fields, Ident};

#[proc_macro_derive(TokenParser)]
pub fn derive_parse_token(input: TokenStream) -> TokenStream {
    let item = parse_macro_input!(input as DeriveInput);
    let enum_name = item.ident;
    let enum_variants = extract_enum_fields(&item.data);
    let prev_quote = Rc::new(RefCell::new(false));
    let token_lifetime_parameter = Rc::new(RefCell::new(Option::<proc_macro2::TokenStream>::None));
    let parser_functions = enum_variants.iter().map(|(variant_name, fields)| {
        let parser_name = format_ident!(
            "{}",
            variant_name.to_string().to_case(convert_case::Case::Snake)
        );
        let return_type_stream = match fields {
            Fields::Named(_) => panic!("named enum variant is not supported."),
            Fields::Unnamed(fields) => {
                if fields.unnamed.len() == 1 {
                    let field = fields.unnamed.first().unwrap();
                    let ty = &field.ty;
                    quote! { &#ty }
                } else {
                    // when tuple
                    let type_tokens_stream = fields.unnamed.iter().map(|field| {
                        let ty = &field.ty;
                        quote! { #ty }
                    });
                    quote! {
                        &(#(#type_tokens_stream),*)
                    }
                }
            }
            Fields::Unit => {
                quote! { () }
            }
        }
        .into_iter()
        .map(|tok| {
            let is_prev_quote = *prev_quote.borrow();
            if is_prev_quote {
                *prev_quote.borrow_mut() = false;
            }
            if let TokenTree::Ident(i) = &tok {
                if is_prev_quote {
                    let quote = TokenTree::Punct(Punct::new('\'', Spacing::Joint));
                    *token_lifetime_parameter.borrow_mut() = Some(quote! {
                        #quote #i
                    });
                }
            }
            if let TokenTree::Punct(p) = &tok {
                if p.as_char() == '\'' {
                    *prev_quote.borrow_mut() = true;
                }
            }
            tok
        })
        .collect::<proc_macro2::TokenStream>();

        let token_life_parameter = if let Some(l) = token_lifetime_parameter.borrow().clone() {
            l
        } else {
            if item.generics.params.len() > 0 {
                quote! { 'a }
            } else {
                quote! {}
            }
        };
        let token_life_parameter_with_comma = if token_life_parameter.is_empty() {
            quote! {}
        } else {
            quote! { #token_life_parameter , }
        };
        let token_life_parameter_with_angles = if token_life_parameter.is_empty() {
            quote! {}
        } else {
            quote! { < #token_life_parameter > }
        };

        let pattern_match_stream = match fields {
            Fields::Named(_) => panic!("named enum variant is not supported."),
            Fields::Unnamed(fields) => {
                let names = (1..=fields.unnamed.len()).map(|i| {
                    let binding_name = format_ident!("_{}", i);
                    quote! { #binding_name }
                });
                quote! {
                    (#(#names),*)
                }
            }
            Fields::Unit => quote! {},
        };
        let tuple_value_stream = match fields {
            Fields::Named(_) => panic!("named enum variant is not supported."),
            Fields::Unnamed(fields) => {
                let names = (1..=fields.unnamed.len()).map(|i| {
                    let binding_name = format_ident!("_{}", i);
                    quote! { #binding_name }
                });
                quote! {
                    (#(#names),*)
                }
            }
            Fields::Unit => quote! { () },
        };

        let lower_variant_name = variant_name
            .to_string()
            .to_case(convert_case::Case::Snake)
            .replace("_", " ");
        let ret = quote! {
            pub fn #parser_name<#token_life_parameter_with_comma W>(
                tokens: & #token_life_parameter [W],
            ) -> picktok::ParseResult<#token_life_parameter, W, #return_type_stream>
            where
                W: Clone + picktok::UnwrapToken<#enum_name #token_life_parameter_with_angles>
            {
                if tokens.is_empty() {
                    return Err(picktok::ParseError {
                        errors: vec![
                            picktok::ParseErrorKind::NotEnoughToken
                        ],
                        tokens_consumed: 0
                    })
                }
                let wrapped_token = &tokens[0];
                let token = wrapped_token.unwrap_token();
                if let #enum_name::#variant_name #pattern_match_stream = token {
                    Ok((&tokens[1..], #tuple_value_stream))
                } else {
                    Err(picktok::ParseError {
                        errors: vec![picktok::ParseErrorKind::Expects {
                            expects: #lower_variant_name,
                            found: wrapped_token.clone()
                        }],
                        tokens_consumed: 0,
                    })
                }
            }
            };
        ret
    });

    let expanded = quote! {
        pub mod parser {
            use super::*;
            #(#parser_functions)*
        }
    };
    TokenStream::from(expanded)
}

fn extract_enum_fields(data: &Data) -> Vec<(&Ident, &Fields)> {
    match *data {
        Data::Enum(ref data) => data
            .variants
            .iter()
            .map(|variant| (&variant.ident, &variant.fields))
            .collect::<Vec<_>>(),
        _ => panic!("invalid data"),
    }
}
