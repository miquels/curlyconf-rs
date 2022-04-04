use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

mod de;

#[proc_macro_derive(CurlyConf, attributes(serde, cc))]
pub fn curlyconf_derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    // Build the trait implementation
    de::impl_curlyconf(ast).into()
}
