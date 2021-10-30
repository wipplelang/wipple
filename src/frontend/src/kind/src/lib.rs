use convert_case::{Case, Casing};
use proc_macro::TokenStream;
use proc_macro2::{Ident, TokenTree};
use quote::quote;
use syn::{parse_macro_input, Fields, FieldsNamed, FnArg, ItemEnum, Pat, Signature};

#[proc_macro_attribute]
pub fn kind(args: TokenStream, input: TokenStream) -> TokenStream {
    let mut args = proc_macro2::TokenStream::from(args).into_iter();

    let item_ident = match args.next() {
        Some(TokenTree::Ident(ident)) => ident,
        _ => panic!("Expected ident"),
    };

    args.next().expect("Expected ':'");
    args.next().expect("Expected ':'");

    let args = args.collect::<proc_macro2::TokenStream>();
    let args = quote! { fn #args }.into();

    let new_fn = parse_macro_input!(args as Signature);

    let new_fn_ident = new_fn.ident;

    let new_fn_args = new_fn.inputs.into_iter().map(|arg| match arg {
        FnArg::Receiver(_) => panic!("Unexpected receiver"),
        FnArg::Typed(arg) => match *arg.pat {
            Pat::Ident(ident) => (ident, *arg.ty),
            _ => panic!("Unexpected pattern"),
        },
    });

    let new_fn_arg_idents = new_fn_args
        .clone()
        .map(|(ident, _)| ident)
        .collect::<Vec<_>>();

    let new_fn_args = new_fn_args
        .map(|(ident, ty)| quote! { #ident: #ty, })
        .collect::<Vec<_>>();

    let input = parse_macro_input!(input as ItemEnum);

    let input_ident = &input.ident;
    let vis = &input.vis;

    let fns = input.variants.iter().map(|variant| {
        let ident = &variant.ident;

        let name = Ident::new(
            &variant.ident.to_string().to_case(Case::Snake),
            variant.ident.span(),
        );

        let fields = match &variant.fields {
            Fields::Named(fields) => fields.clone(),
            Fields::Unit => FieldsNamed {
                brace_token: Default::default(),
                named: Default::default()
            },
            _ => panic!("Expected fields"),
        };

        let field_idents = fields
            .named
            .iter()
            .map(|field| field.ident.as_ref().unwrap());

        let params = fields.named.iter().map(|field| {
            let ident = field.ident.as_ref().unwrap();
            let ty = &field.ty;

            quote! { #ident: #ty, }
        });

        let comma = (!new_fn_arg_idents.is_empty()).then(|| quote!(,));

        quote! {
            #vis fn #name(#(#new_fn_args)* #(#params)*) -> Self {
                #item_ident::#new_fn_ident(#(#new_fn_arg_idents),* #comma #input_ident::#ident { #(#field_idents),* })
            }
        }
    });

    let r#impl = quote! {
        #input

        impl #item_ident {
            #(#fns)*
        }
    };

    r#impl.into()
}
