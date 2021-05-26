#![crate_type = "proc-macro"]

use proc_macro::TokenStream;
use quote::quote;

#[proc_macro_attribute]
pub fn wipple_plugin_attribute(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let item = syn::parse::<syn::ItemFn>(item).unwrap();

    let fn_name = &item.sig.ident;

    let result = quote! {
        #item

        #[no_mangle]
        pub extern "C" fn _wipple_plugin(
            env: &wipple::Environment,
            stack: &wipple::Stack,
        ) -> Box<Result> {
            Box::new(#fn_name(env, stack))
        }
    };

    result.into()
}
