#![crate_type = "proc-macro"]

use proc_macro::TokenStream;
use quote::quote;

#[proc_macro_derive(TypeInfo)]
pub fn typeinfo(item: TokenStream) -> TokenStream {
    let mut item = syn::parse::<syn::Item>(item).unwrap();

    let (ident, generics) = match &mut item {
        syn::Item::Struct(item) => (&item.ident, &mut item.generics),
        syn::Item::Enum(item) => (&item.ident, &mut item.generics),
        syn::Item::Union(item) => (&item.ident, &mut item.generics),
        _ => unreachable!(),
    };

    let (impl_generics, ty_generics, _) = generics.split_for_impl();
    let mut where_clause = generics.clone().make_where_clause().clone();

    for param in &generics.params {
        let param = match param {
            syn::GenericParam::Type(param) => param,
            _ => continue,
        };

        let ident = &param.ident;

        let predicate =
            syn::parse::<syn::WherePredicate>((quote! { #ident: TypeInfo }).into()).unwrap();

        where_clause.predicates.push(predicate);
    }

    let generics = generics.params.iter().filter_map(|param| match param {
        syn::GenericParam::Type(ty) => {
            let ident = &ty.ident;

            Some(quote! { <#ident>::DYNAMIC_TYPE })
        }
        _ => None,
    });

    let result = quote! {
        impl #impl_generics dynamic::TypeInfo for #ident #ty_generics #where_clause {
            const DYNAMIC_TYPE: dynamic::DynamicType = dynamic::DynamicType {
                crate_name: env!("CARGO_PKG_NAME"),
                crate_version: env!("CARGO_PKG_VERSION"),
                module: module_path!(),
                type_name: stringify!(#ident),
                generics: &[#(#generics),*],
            };
        }
    };

    result.into()
}
