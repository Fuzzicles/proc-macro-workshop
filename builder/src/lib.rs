use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
    parse_macro_input, Data, DataStruct, DeriveInput, Fields, FieldsNamed, Ident, Type, Visibility, TypePath, Path, GenericArgument,
};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {

    let input = parse_macro_input!(input as DeriveInput);
    let struct_name = input.ident;
    let builder_name = format_ident!("{}Builder", struct_name);

    let field_data = match input.data {
        Data::Struct(data) => parse_struct(data),
        _ => unimplemented!("Not implemented for non-structs"),
    };

    let builder_fields = field_data.iter()
        .map(|(ident, ty, vis)| {
            match get_optional(&ty) {
                Some(optional_ident) => quote! {
                    #ident: #ty
                },
                None =>  quote! {
                    #ident: Option<#ty>
                }
            }
        });

    let builder_init_fields = field_data.iter()
        .map(|(ident, ty, vis)| {
            quote! {
                #ident: None
            }
        });
    
    let builder_setters = field_data.iter()
        .map(|(ident, ty, vis)|  {

            match get_optional(ty) {
                Some(option_type) =>{
                    quote! {
                        fn #ident(&mut self, #ident: #option_type) -> &mut Self {
                            self.#ident = Some(#ident);
                            self
                        }
                    }
                },
                None => quote! {
                    fn #ident(&mut self, #ident: #ty) -> &mut Self {
                        self.#ident = Some(#ident);
                        self
                    }
                }
            }
        });

    let build_body = field_data.iter()
        .map(|(ident, ty, vis)| {
            match get_optional(&ty) {
                Some(option_type) => {
                    quote! {
                        let #ident: #ty = self.#ident.clone();
                    }
                },
                None => {
                    quote! {
                        let #ident: #ty = match &self.#ident {
                            Some(val) => val.clone(),
                            None => return Err(Box::<dyn std::error::Error>::from(String::from("Missing a required field")))
                        };
                    }
                }
            }
        });

    let build_fields = field_data.iter()
        .map(|(ident, ty, vis)| {
            quote! {
                #ident
            }
        });

    let builder_struct = quote! {
        pub struct #builder_name {
            #( #builder_fields),*
        }
    };

    let builder_impl = quote! {
        impl #builder_name {

            #( #builder_setters)*

            pub fn build(&mut self) -> Result<#struct_name, Box<dyn std::error::Error>> {
                #( #build_body)*
                Ok(#struct_name {
                    #( #build_fields),*
                })
            }
        }
    };

    let expanded = quote! {

        #builder_struct

        #builder_impl

        impl #struct_name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #( #builder_init_fields),*
                }
            }

        }

    };
    TokenStream::from(expanded)
}

fn parse_struct(data: DataStruct) -> Vec<(Ident, Type, Visibility)> {
    match data.fields {
        Fields::Named(fields) => parse_named_fields(fields),
        _ => unimplemented!("Not implemented for structs without named fields"),
    }
}

fn parse_named_fields(fields: FieldsNamed) -> Vec<(Ident, Type, Visibility)> {
    let mut field_data = Vec::new();
    for field in fields.named {
        field_data.push((
            field.ident.unwrap(),
            field.ty,
            field.vis
        ));
    }
    field_data
}

// Returns Some(Ident) where Ident is the inner type of the Option
// Returns None if the type is not an Option
fn get_optional(ty: &Type) -> Option<&Ident> {
    match ty {
        Type::Path(TypePath {
            qself: None, 
            path: Path {
                leading_colon: None,
                segments
            }
        }) => {
            match segments.iter().collect::<Vec<_>>()[..] {
                [segment] => match &segment.ident.to_string() as &str {
                    "Option" => match &segment.arguments {
                        syn::PathArguments::AngleBracketed(args) => {
                            match args.args.iter().collect::<Vec<_>>()[..] {
                                [GenericArgument::Type(ty)] => {
                                    match ty {
                                        Type::Path(TypePath {
                                            qself: None,
                                            path: Path {
                                                leading_colon: None,
                                                segments
                                            }
                                        }) => match segments.iter().collect::<Vec<_>>()[..] {
                                            [segment] => Some(&segment.ident),
                                            _ => unimplemented!("Multiple subpaths")
                                        },
                                        _ => unimplemented!("Non typepath")
                                    }
                                },
                                _ => todo!("Handle options with multiple generic args"),
                            }
                        },
                        // Option can only be followed by angle brackets (I think) so this shouldn't be reachable
                        _ => unreachable!(),
                    },
                    _ => None,
                },
                _ => todo!("Handle cases other than precisely 1 segment")
            }
        },
        _ => unimplemented!("Looking for a specfic syntax tree, and no others")
    }
}
