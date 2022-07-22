#[cfg(debug_assertions)]
mod runtime;

use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use std::{collections::BTreeMap, mem};
use thiserror::Error;
use wipple_frontend::{helpers::InternedString, ir, ComputationId, TypeId, VariableId};

#[derive(Debug, Clone, Error)]
pub enum Error {
    #[error("unknown abi `{0}`")]
    UnknownAbi(InternedString),
}

pub type Result<T> = std::result::Result<T, Error>;

pub fn compile(ir: &ir::Program) -> Result<TokenStream> {
    let mut info = Info {
        program: ir,
        computations: Default::default(),
        types: Default::default(),
    };

    let runtime = syn::parse_str::<TokenStream>(include_str!("./runtime.rs")).unwrap();

    let constants = ir
        .constants
        .iter()
        .enumerate()
        .map(|(id, constant)| {
            let id = write_constant(id);
            let ty = write_type(&constant.ty, &mut info);

            let init = write_sections(&constant.sections, false, &mut info)?;

            Ok(quote! {
                static #id: #ty = {
                    #init
                };
            })
        })
        .collect::<Result<Vec<_>>>()?;

    let entrypoint = write_sections(&ir.entrypoint, true, &mut info)?;

    let types = info.types.into_values().map(|(_, decl)| decl.unwrap());

    Ok(quote! {
        #runtime

        #(#types)*

        thread_local! {
            #(#constants)*
        }

        fn main() {
            #entrypoint
        }
    })
}

struct Info<'a> {
    program: &'a ir::Program,
    computations: BTreeMap<ComputationId, &'a ir::Type>,
    types: BTreeMap<TypeId, (TokenStream, Option<TokenStream>)>,
}

fn write_sections<'a>(
    sections: &'a ir::Sections,
    entrypoint: bool,
    info: &mut Info<'a>,
) -> Result<TokenStream> {
    let prev_computations = mem::take(&mut info.computations);

    let mut labels = Vec::new();
    let mut variables = Vec::new();

    let sections = sections
        .enumerate()
        .map(|(index, section)| {
            let label = write_section(index);
            labels.push(label.clone());

            let statements = section
                .statements
                .iter()
                .map(|statement| {
                    Ok(match statement {
                        ir::Statement::Compute(id, expr) => {
                            info.computations.insert(*id, &expr.ty);

                            let id = write_computation(*id);
                            let expr = write_expression(expr, info)?;

                            quote!(__runtime::init(&mut #id, #expr);)
                        }
                        ir::Statement::Initialize(var, computation) => {
                            variables.push((*var, *computation));

                            let var = write_variable(*var);
                            let ty = write_type(info.computations.get(computation).unwrap(), info);
                            let computation = write_computation(*computation);

                            quote!(__runtime::init(&mut #var, __runtime::read::<_, #ty>(&#computation));)
                        }
                        ir::Statement::Drop(vars) => {
                            let vars = vars.iter().map(|var| {
                                let var = write_variable(*var);
                                quote!(__runtime::drop(&mut #var);)
                            });

                            quote!(#(#vars)*)
                        }
                    })
                })
                .collect::<Result<Vec<_>>>()?;

            let terminator = section
                .terminator
                .as_ref()
                .map(|terminator| match terminator {
                    ir::Terminator::If(condition, then_branch, else_branch) => {
                        let ty = write_type(info.computations.get(condition).unwrap(), info);
                        let condition = write_computation(*condition);
                        let then_branch = write_section(*then_branch);
                        let else_branch = write_section(*else_branch);

                        quote! {
                            __label = if __runtime::read::<_, #ty>(&#condition) {
                                __label::#then_branch
                            } else {
                                __label::#else_branch
                            };

                            continue;
                        }
                    }
                    ir::Terminator::Return(computation) => {
                        let ty = write_type(info.computations.get(computation).unwrap(), info);
                        let computation = write_computation(*computation);
                        quote!(break __runtime::read::<_, #ty>(&#computation);)
                    }
                    ir::Terminator::Goto(index) => {
                        let label = write_section(*index);
                        quote! {
                            __label = __label::#label;
                            continue;
                        }
                    }
                    ir::Terminator::Unreachable => quote!(__runtime::unreachable();),
                })
                .or_else(|| entrypoint.then(|| quote!(break;)));

            Ok(quote! {
                __label::#label => {
                    #(#statements)*
                    #terminator
                }
            })
        })
        .collect::<Result<Vec<_>>>()?;

    let variables = variables
        .into_iter()
        .map(|(var, computation)| {
            let ty = write_type(info.computations.get(&computation).unwrap(), info);
            let var = write_variable(var);

            quote!(let mut #var: __runtime::Value<#ty> = __runtime::value();)
        })
        .collect::<Vec<_>>();

    let computations = mem::replace(&mut info.computations, prev_computations)
        .into_iter()
        .map(|(id, ty)| {
            let id = write_computation(id);
            let ty = write_type(ty, info);

            quote!(let mut #id: __runtime::Value<#ty> = __runtime::value();)
        });

    let first_label = write_section(ir::SectionIndex(0));

    Ok(quote! {
        enum __label {
            #(#labels,)*
        }

        unsafe {
            #(#variables)*
            #(#computations)*

            let mut __label = __label::#first_label;
            loop {
                match __label {
                    #(#sections)*
                }
            }
        }
    })
}

fn write_expression<'a>(expr: &'a ir::Expression, info: &mut Info<'a>) -> Result<TokenStream> {
    Ok(match &expr.kind {
        ir::ExpressionKind::Marker => quote!(__runtime::Marker),
        ir::ExpressionKind::Constant(id) => {
            let id = write_constant(*id);
            let ty = write_type(&expr.ty, info);
            quote!(__runtime::read_init::<_, #ty>(&#id))
        }
        ir::ExpressionKind::Function(function) => {
            let outer_captures = function.captures.iter().map(|id| {
                let id = write_variable(*id);
                quote!(let #id = __runtime::capture(&#id);)
            });

            let inner_captures = function.captures.iter().map(|id| {
                let id = write_variable(*id);
                quote!(let mut #id = __runtime::capture(&#id);)
            });

            let (input_ty, output_ty) = match &expr.ty {
                ir::Type::Function(input, output) => {
                    (write_type(input, info), write_type(output, info))
                }
                _ => unreachable!(),
            };

            let sections = write_sections(&function.sections, false, info)?;

            quote! {
                __runtime::function({
                    #(#outer_captures)*

                    move |__function_input: #input_ty| -> #output_ty {
                        #(#inner_captures)*
                        #sections
                    }
                })
            }
        }
        ir::ExpressionKind::Variable(id) => {
            let ty = write_type(&expr.ty, info);
            let id = write_variable(*id);
            quote!(__runtime::read::<_, #ty>(&#id))
        }
        ir::ExpressionKind::FunctionInput => quote!(__function_input.clone()),
        ir::ExpressionKind::Number(number) => quote!(#number),
        ir::ExpressionKind::Text(text) => {
            let text = text.as_str();
            quote!(__runtime::text(#text))
        }
        ir::ExpressionKind::Call(function, input) => {
            let function_ty = write_type(info.computations.get(function).unwrap(), info);
            let input_ty = write_type(info.computations.get(input).unwrap(), info);
            let function = write_computation(*function);
            let input = write_computation(*input);

            quote! {
                __runtime::call(
                    __runtime::read::<_, #function_ty>(&#function),
                    __runtime::read::<_, #input_ty>(&#input),
                )
            }
        }
        ir::ExpressionKind::External(abi, identifier, inputs) => match abi.as_str() {
            "runtime" => {
                let function = syn::Ident::new(&identifier.replace('-', "_"), Span::call_site());

                let inputs = inputs.iter().map(|id| {
                    let ty = write_type(info.computations.get(id).unwrap(), info);
                    let id = write_computation(*id);
                    quote!(__runtime::read::<_, #ty>(&#id))
                });

                quote!(__runtime::builtins::#function(#(#inputs),*))
            }
            "rust" => todo!(),
            "c" => todo!(),
            _ => return Err(Error::UnknownAbi(*abi)),
        },
        ir::ExpressionKind::Structure(values) => {
            let id = match &expr.ty {
                ir::Type::Structure(id, _) => write_structure(*id),
                _ => unreachable!(),
            };

            let fields = values.iter().enumerate().map(|(index, id)| {
                let field = write_structure_field(index);
                let ty = write_type(info.computations.get(id).unwrap(), info);
                let id = write_computation(*id);
                quote!(#field: __runtime::read::<_, #ty>(&#id))
            });

            quote! {
                #id {
                    #(#fields,)*
                }
            }
        }
        ir::ExpressionKind::Tuple(values) => {
            let values = values.iter().map(|id| {
                let ty = write_type(info.computations.get(id).unwrap(), info);
                let id = write_computation(*id);
                quote!(__runtime::read::<_, #ty>(&#id))
            });

            quote!((#(#values,)*))
        }
        ir::ExpressionKind::Variant(discriminant, values) => {
            let id = match &expr.ty {
                ir::Type::Enumeration(id, _) => write_enumeration(*id),
                _ => unreachable!(),
            };

            let variant = write_enumeration_variant(*discriminant);

            let values = values.iter().map(|id| {
                let ty = write_type(info.computations.get(id).unwrap(), info);
                let id = write_computation(*id);
                quote!(__runtime::read::<_, #ty>(&#id))
            });

            quote!(#id::#variant(#(#values),*))
        }
        ir::ExpressionKind::TupleElement(computation, index) => {
            let ty = write_type(info.computations.get(computation).unwrap(), info);
            let computation = write_computation(*computation);
            let index = proc_macro2::Literal::usize_unsuffixed(*index);
            quote!(__runtime::read::<_, #ty>(&#computation).#index)
        }
        ir::ExpressionKind::StructureElement(computation, _, index) => {
            let ty = write_type(info.computations.get(computation).unwrap(), info);
            let computation = write_computation(*computation);
            let field = write_structure_field(*index);
            quote!(__runtime::read::<_, #ty>(&#computation).#field)
        }
        ir::ExpressionKind::VariantElement(computation, (id, discriminant), index) => {
            let ty = write_type(info.computations.get(computation).unwrap(), info);
            let computation = write_computation(*computation);
            let variant = write_enumeration_variant(*discriminant);

            let variants = match info.program.nominal_types.get(id).unwrap() {
                ir::Type::Enumeration(_, variants) => (0..variants[*discriminant].len()).map(|i| {
                    if i == *index {
                        quote!(__element)
                    } else {
                        quote!(_)
                    }
                }),
                _ => unreachable!(),
            };

            let id = write_enumeration(*id);

            quote! {
                match __runtime::read::<_, #ty>(&#computation) {
                    #id::#variant(#(#variants),*) => __element,
                    _ => __runtime::unreachable(),
                }
            }
        }
        ir::ExpressionKind::Discriminant(computation) => {
            let ty = write_type(info.computations.get(computation).unwrap(), info);
            let computation = write_computation(*computation);
            quote!(__runtime::discriminant(&__runtime::read::<_, #ty>(&#computation)))
        }
        ir::ExpressionKind::CompareDiscriminants(computation, discriminant) => {
            let ty = write_type(info.computations.get(computation).unwrap(), info);
            let computation = write_computation(*computation);
            quote!(__runtime::read::<_, #ty>(&#computation) == #discriminant)
        }
    })
}

fn write_type(ty: &ir::Type, info: &mut Info) -> TokenStream {
    match ty {
        ir::Type::Number => quote!(__runtime::Number),
        ir::Type::Text => quote!(__runtime::Text),
        ir::Type::Discriminant => quote!(__runtime::Discriminant),
        ir::Type::Boolean => quote!(__runtime::Boolean),
        ir::Type::Function(input, output) => {
            let input = write_type(input, info);
            let output = write_type(output, info);
            quote!(__runtime::Function<#input, #output>)
        }
        ir::Type::Tuple(tys) => {
            let tys = tys.iter().map(|ty| write_type(ty, info));
            quote!((#(#tys,)*))
        }
        ir::Type::List(ty) => {
            let ty = write_type(ty, info);
            quote!(__runtime::List<#ty>)
        }
        ir::Type::Mutable(ty) => {
            let ty = write_type(ty, info);
            quote!(__runtime::Mutable<#ty>)
        }
        ir::Type::Marker => quote!(__runtime::Marker),
        ir::Type::Structure(id, fields) => {
            let name = write_structure(*id);

            if !info.types.contains_key(id) {
                info.types.insert(*id, (name.clone(), None));

                let fields = fields.iter().enumerate().map(|(index, ty)| {
                    let field = write_structure_field(index);
                    let ty = write_type(ty, info);

                    quote!(#field: #ty,)
                });

                let decl = quote! {
                    #[derive(Clone)]
                    struct #name {
                        #(#fields)*
                    }
                };

                info.types.get_mut(id).unwrap().1 = Some(decl);
            }

            name
        }
        ir::Type::Enumeration(id, variants) => {
            let name = write_enumeration(*id);

            if !info.types.contains_key(id) {
                info.types.insert(*id, (name.clone(), None));

                let discriminants = variants.iter().enumerate().map(|(index, _)| {
                    let variant = write_enumeration_variant(index);

                    quote!(#name::#variant(..) => #index,)
                });

                let variants = variants.iter().enumerate().map(|(index, tys)| {
                    let variant = write_enumeration_variant(index);
                    let tys = tys.iter().map(|ty| write_type(ty, info));

                    quote!(#variant(#(#tys),*),)
                });

                let decl = quote! {
                    #[derive(::std::clone::Clone)]
                    enum #name {
                        #(#variants)*
                    }

                    impl __runtime::Enumeration for #name {
                        fn discriminant(&self) -> __runtime::Discriminant {
                            match self {
                                #(#discriminants)*
                            }
                        }
                    }
                };

                info.types.get_mut(id).unwrap().1 = Some(decl);
            }

            name
        }
        ir::Type::Unreachable => quote!(__runtime::Unreachable),
    }
}

fn write_section(index: ir::SectionIndex) -> TokenStream {
    syn::Ident::new(&format!("__label_{}", index.0), Span::call_site()).to_token_stream()
}

fn write_computation(computation: ComputationId) -> TokenStream {
    syn::Ident::new(
        &format!("__computation_{}", computation.0),
        Span::call_site(),
    )
    .to_token_stream()
}

fn write_variable(var: VariableId) -> TokenStream {
    syn::Ident::new(&format!("__variable_{}", var.0), Span::call_site()).to_token_stream()
}

fn write_constant(constant: usize) -> TokenStream {
    syn::Ident::new(&format!("__constant_{}", constant), Span::call_site()).to_token_stream()
}

fn write_structure(id: TypeId) -> TokenStream {
    syn::Ident::new(&format!("__structure_{}", id.0), Span::call_site()).to_token_stream()
}

fn write_structure_field(index: usize) -> TokenStream {
    syn::Ident::new(&format!("__field_{}", index), Span::call_site()).to_token_stream()
}

fn write_enumeration(id: TypeId) -> TokenStream {
    syn::Ident::new(&format!("__enumeration_{}", id.0), Span::call_site()).to_token_stream()
}

fn write_enumeration_variant(index: usize) -> TokenStream {
    syn::Ident::new(&format!("__variant_{}", index), Span::call_site()).to_token_stream()
}
