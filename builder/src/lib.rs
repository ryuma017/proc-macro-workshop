use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, Data, DataStruct, DeriveInput, Field,
    Fields, GenericArgument, Path, PathArguments, Type, TypePath,
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
    // eprintln!("builder_fields: {:#?}", builder_fields);
    let builder_setter_fns = fields
        .named
        .iter()
        .map(create_builder_setter_fn)
        .collect::<Vec<TokenStream>>();
    let builder_default_fields = fields
        .named
        .iter()
        .map(create_builder_default_field)
        .collect::<Vec<TokenStream>>();
    let builder_build_fields = fields
        .named
        .iter()
        .map(create_builder_build_field)
        .collect::<Vec<TokenStream>>();

    let expanded = quote! {
        pub struct #builder_ident {
            #(#builder_fields,)*
        }

        impl #builder_ident {
            #(#builder_setter_fns)*

            pub fn build(&mut self) -> ::std::result::Result<#ident, ::std::boxed::Box<dyn ::std::error::Error>> {
                ::std::result::Result::Ok(#ident {
                    #(#builder_build_fields,)*
                })
            }
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
    // Ok(TokenStream::new())
}

fn create_builder_field(field: &Field) -> TokenStream {
    let ident = &field.ident;
    let ty = &field.ty;
    if unwrap_ty(ty, "Option").is_some() {
        quote! { #ident: #ty }
    } else {
        quote! { #ident: ::std::option::Option<#ty> }
    }
}

fn create_builder_setter_fn(field: &Field) -> TokenStream {
    let ident = &field.ident;
    let ty = &field.ty;
    if let Some(option_inner_type) = unwrap_ty(ty, "Option") {
        quote! {
            pub fn #ident(&mut self, #ident: #option_inner_type) -> &mut Self {
                self.#ident = ::std::option::Option::Some(#ident);
                self
            }
        }
    } else {
        quote! {
            pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                self.#ident = ::std::option::Option::Some(#ident);
                self
            }
        }
    }
}

fn create_builder_default_field(field: &Field) -> TokenStream {
    let ident = &field.ident;
    quote! { #ident: ::std::option::Option::None }
}

fn create_builder_build_field(field: &Field) -> TokenStream {
    let ident = &field.ident;
    let ty = &field.ty;
    if unwrap_ty(ty, "Option").is_some() {
        quote! {
            #ident: self.#ident.clone()
        }
    } else {
        quote! {
            #ident: self.#ident.clone().ok_or(concat!(stringify!(#ident), " is not set."))?
        }
    }
}

// ex.) Option<String> -> Some("String"), Vec<String> -> Some("String")
fn unwrap_ty<'a>(ty: &'a Type, wrapper: &str) -> Option<&'a Type> {
    match ty {
        Type::Path(TypePath {
            path: Path { segments, .. },
            ..
        }) => segments.last().and_then(|path_segment| {
            path_segment
                .ident
                .eq(wrapper)
                .then(|| match path_segment.arguments {
                    PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                        ref args,
                        ..
                    }) => args.first().and_then(|arg| match arg {
                        GenericArgument::Type(ref ty) => Some(ty),
                        _ => None,
                    }),
                    _ => None,
                })
                .unwrap_or(None)
        }),
        _ => None,
    }
}
