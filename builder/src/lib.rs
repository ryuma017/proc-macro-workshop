use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, Data, DataStruct, DeriveInput, Error, Field,
    Fields, GenericArgument, LitStr, Path, PathArguments, Type, TypePath,
};

const ATTR_NAME: &str = "builder";
const EACH_ATTR_NAME: &str = "each";

type Result<T> = std::result::Result<T, Error>;

#[proc_macro_derive(Builder, attributes(builder))]
pub fn builder(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    expand_derive_builder(input)
        .unwrap_or_else(|e| e.to_compile_error())
        .into()
}

fn expand_derive_builder(input: DeriveInput) -> Result<TokenStream> {
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

    let builder_setter_fns = fields
        .named
        .iter()
        .filter_map(create_builder_setter_fn)
        .collect::<Vec<TokenStream>>();

    let builder_each_setter_fns = fields
        .named
        .iter()
        .filter_map(create_builder_each_setter_fn)
        .collect::<Result<Vec<TokenStream>>>()?;

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

            #(#builder_each_setter_fns)*

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
}

fn create_builder_field(field: &Field) -> TokenStream {
    let ident = &field.ident;
    let ty = &field.ty;
    if inner_ty(ty, "Option").is_some() {
        quote! { #ident: #ty }
    } else {
        quote! { #ident: ::std::option::Option<#ty> }
    }
}

fn create_builder_setter_fn(field: &Field) -> Option<TokenStream> {
    let ident = &field.ident;
    let ty = &field.ty;
    if let (Some(Ok(v)), Some(ident)) = (extract_each_value(field), ident) {
        if format_ident!("{}", v.value()) == *ident {
            return None;
        }
    }

    if let Some(option_inner_type) = inner_ty(ty, "Option") {
        Some(quote! {
            pub fn #ident(&mut self, #ident: #option_inner_type) -> &mut Self {
                self.#ident = ::std::option::Option::Some(#ident);
                self
            }
        })
    } else {
        Some(quote! {
            pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                self.#ident = ::std::option::Option::Some(#ident);
                self
            }
        })
    }
}

fn create_builder_each_setter_fn(field: &Field) -> Option<Result<TokenStream>> {
    let ident = &field.ident;
    let vec_inner_ty = inner_ty(&field.ty, "Vec")?;
    let each_value_litstr = extract_each_value(field);

    match each_value_litstr {
        Some(Ok(litstr)) => {
            let each_value_ident = format_ident!("{}", litstr.value());
            Some(Ok(quote! {
                pub fn #each_value_ident(&mut self, #each_value_ident: #vec_inner_ty) -> &mut Self {
                    match self.#ident {
                        ::std::option::Option::Some(ref mut v) => v.push(#each_value_ident),
                        ::std::option::Option::None => {
                            self.#ident = ::std::option::Option::Some(
                                vec![#each_value_ident]
                            );
                        }
                    }
                    self
                }
            }))
        }
        Some(Err(e)) => Some(Err(e)),
        None => None,
    }
}

fn create_builder_default_field(field: &Field) -> TokenStream {
    let ident = &field.ident;
    quote! { #ident: ::std::option::Option::None }
}

fn create_builder_build_field(field: &Field) -> TokenStream {
    let ident = &field.ident;
    let ty = &field.ty;
    if inner_ty(ty, "Option").is_some() {
        quote! {
            #ident: self.#ident.clone()
        }
    } else if inner_ty(ty, "Vec").is_some() {
        quote! {
            #ident: self.#ident.clone().unwrap_or_default()
        }
    } else {
        quote! {
            #ident: self.#ident.clone().ok_or(concat!(stringify!(#ident), " is not set."))?
        }
    }
}

// ex.) Option<String> -> Some("String"), Vec<String> -> Some("String")
fn inner_ty<'a>(ty: &'a Type, wrapper: &str) -> Option<&'a Type> {
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

fn extract_each_value(field: &Field) -> Option<Result<LitStr>> {
    field.attrs.iter().find_map(|attr| {
        if attr.path().is_ident(ATTR_NAME) {
            let mut litstr = None::<LitStr>;
            attr.parse_nested_meta(|meta| {
                if meta.path.is_ident(EACH_ATTR_NAME) {
                    let v = meta.value()?;
                    litstr = Some(v.parse::<LitStr>()?);
                    Ok(())
                } else {
                    Err(Error::new_spanned(
                        &attr.meta,
                        format!("expected `{ATTR_NAME}({EACH_ATTR_NAME} = \"...\")`"),
                    ))
                }
            })
            .map(|_| litstr)
            .transpose()
        } else {
            None
        }
    })
}
