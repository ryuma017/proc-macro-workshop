use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let ident = &input.ident;
    let builder_ident = syn::Ident::new(&format!("{}Builder", ident), ident.span());
    let expanded = quote!{
        pub struct #builder_ident {}

        impl #ident {
            pub fn builder() -> #builder_ident {
                #builder_ident {}
            }
        }
    };
    expanded.into()
}
