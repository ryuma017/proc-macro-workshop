use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{
    parse_macro_input, Data, DataStruct, DeriveInput, Field, Fields, Path, PathSegment, Type,
    TypePath,
};

#[proc_macro_derive(Builder)]
pub fn builder(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    expand_derive_builder(input)
        .unwrap_or_else(|e| e.to_compile_error())
        .into()
}

fn expand_derive_builder(input: DeriveInput) -> Result<TokenStream, syn::Error> {
    let ident = &input.ident;
    let fields = match &input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(named_fields),
            ..
        }) => named_fields,
        _ => {
            return Err(syn::Error::new_spanned(
                input,
                "expected a struct that has named fields",
            ))
        }
    };

    let builder_ident = format_ident!("{}Builder", ident, span = ident.span());
    let builder_fields = fields
        .named
        .iter()
        .map(create_builder_field)
        .collect::<Vec<TokenStream>>();
    let builder_setters = fields
        .named
        .iter()
        .map(create_builder_setter)
        .collect::<Vec<TokenStream>>();
    let builder_default_fields = fields
        .named
        .iter()
        .map(create_builder_default_field)
        .collect::<Vec<TokenStream>>();

    let expanded = quote! {
        pub struct #builder_ident {
            #(#builder_fields,)*
        }

        impl #builder_ident {
            #(#builder_setters)*
        }

        impl #ident {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #(#builder_default_fields,)*
                }
            }
        }
    };

    Ok(expanded)
}

fn create_builder_field(field: &Field) -> TokenStream {
    let ident = &field.ident;
    let ty = &field.ty;
    if ty_base_ident_eq(ty, "Option") {
        quote! { #ident: #ty }
    } else {
        quote! { #ident: ::std::option::Option<#ty> }
    }
}

fn create_builder_setter(field: &Field) -> TokenStream {
    let ident = &field.ident;
    let ty = &field.ty;
    if ty_base_ident_eq(ty, "Option") {
        quote! {
            pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                self.#ident = #ident;
                self
            }
        }
    } else {
        quote! {
            pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                self.#ident = Some(#ident);
                self
            }
        }
    }
}

fn create_builder_default_field(field: &Field) -> TokenStream {
    let ident = &field.ident;
    quote! { #ident: ::std::option::Option::None }
}

fn ty_base_ident_eq(ty: &Type, ident: &str) -> bool {
    if let Type::Path(TypePath { path, .. }) = ty {
        path.segments.last()
    } else {
        None
    }
    .map(|seg| seg.ident == ident)
    .unwrap_or(false)
}
