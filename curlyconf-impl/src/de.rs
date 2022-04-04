use proc_macro2::TokenStream;
use quote::quote;
use syn::{Data, DataStruct, DeriveInput, Fields};

pub fn impl_curlyconf(ast: DeriveInput) -> TokenStream {
    let fields = match ast.data {
        Data::Struct(DataStruct { fields: Fields::Named(fields), .. }) => fields.named,
        _ => panic!("this derive macro only works on structs with named fields"),
    };

    let st_name = ast.ident;

    let fdesc = fields.into_iter().map(|f| {
        let field_name = f.ident;
        let field_ty = f.ty;

        quote! {
            desc.insert(stringify!(#field_name).to_string(), <#field_ty>::curlyconf());
        }
    });

    quote! {
        #[automatically_derived]
        impl CurlyConf for #st_name {
            fn describe() -> ::curlyconf::Type {
                let mut desc = ::std::collections::HashMap::new();
                #(#fdesc)*
                ::curlyconf::Type::Struct {
                    key_field: None,
                    fields: desc,
                }
            }
        }
    }
}
