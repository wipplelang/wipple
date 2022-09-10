#[cfg(debug_assertions)]
mod runtime;

use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use std::{collections::BTreeMap, mem};
use thiserror::Error;
use wipple_frontend::{ir, ComputationId, MonomorphizedConstantId, TypeId, VariableId};

#[derive(Debug, Clone, Error)]
pub enum Error {
    #[error("invalid identifier")]
    InvalidIdentifier,
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
        .map(|(id, constant)| {
            let id = write_constant(*id);
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
            let var = write_variable(var);
            let ty = (*info.computations.get(&computation).unwrap()).clone();
            let wrapper = write_wrapper(&ty, info);
            let ty = write_type(&ty, info);

            quote!(let mut #var: #wrapper<#ty> = __runtime::value();)
        })
        .collect::<Vec<_>>();

    let computations = mem::replace(&mut info.computations, prev_computations)
        .into_iter()
        .map(|(computation, ty)| {
            let id = write_computation(computation);
            let wrapper = write_wrapper(ty, info);
            let ty = write_type(ty, info);

            quote!(let mut #id: #wrapper<#ty> = __runtime::value();)
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
        ir::ExpressionKind::Number(number) => {
            let parts = number
                .serialize()
                .into_iter()
                .map(proc_macro2::Literal::u8_suffixed)
                .collect::<Vec<_>>();

            quote!(rust_decimal::Decimal::deserialize([#(#parts),*]))
        }
        ir::ExpressionKind::Integer(integer) => quote!(#integer),
        ir::ExpressionKind::Natural(natural) => quote!(#natural),
        ir::ExpressionKind::Byte(byte) => quote!(#byte),
        ir::ExpressionKind::Signed(signed) => quote!(#signed),
        ir::ExpressionKind::Unsigned(unsigned) => quote!(#unsigned),
        ir::ExpressionKind::Float(float) => quote!(#float),
        ir::ExpressionKind::Double(double) => quote!(#double),
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
        ir::ExpressionKind::External(lib, identifier, inputs) => {
            let lib = lib.as_str();

            if lib == "runtime" {
                let function = syn::Ident::new(&identifier.replace('-', "_"), Span::call_site());

                let inputs = inputs.iter().map(|id| {
                    let ty = write_type(info.computations.get(id).unwrap(), info);
                    let id = write_computation(*id);
                    quote!(__runtime::read::<_, #ty>(&#id))
                });

                quote!(__runtime::builtins::#function(#(#inputs),*))
            } else {
                use nom::{
                    bytes::complete::tag,
                    character::complete::{alphanumeric1, char, space0},
                    multi::separated_list0,
                    sequence::delimited,
                    Parser,
                };

                #[derive(Debug, Clone, PartialEq, Eq)]
                enum Ty {
                    Int,
                    Double,
                }

                fn ty(s: &str) -> nom::IResult<&str, Ty> {
                    (tag("int").map(|_| Ty::Int))
                        .or(tag("double").map(|_| Ty::Double))
                        .parse(s)
                }

                impl std::fmt::Display for Ty {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                        match self {
                            Ty::Int => write!(f, "int"),
                            Ty::Double => write!(f, "double"),
                        }
                    }
                }

                impl Ty {
                    fn c_ty(&self) -> TokenStream {
                        match self {
                            Ty::Int => quote!(::std::os::raw::c_int),
                            Ty::Double => quote!(::std::os::raw::c_double),
                        }
                    }
                }

                fn return_ty(s: &str) -> nom::IResult<&str, Option<Ty>> {
                    (ty.map(Some)).or(tag("void").map(|_| None)).parse(s)
                }

                fn args(s: &str) -> nom::IResult<&str, Vec<Ty>> {
                    delimited(
                        char('(').and(space0),
                        separated_list0(space0.and(tag(",")).and(space0), ty),
                        space0.and(char(')')),
                    )(s)
                }

                let (rest, ((((return_ty, _), func), _), args)) = return_ty
                    .and(space0)
                    .and(alphanumeric1)
                    .and(space0)
                    .and(args)
                    .parse(identifier)
                    .map_err(|_| Error::InvalidIdentifier)?;

                if !rest.is_empty() {
                    return Err(Error::InvalidIdentifier);
                }

                let func = syn::Ident::new(func, Span::call_site());

                let inputs = args.iter().zip(inputs).map(|(arg, input)| {
                    let arg_ty = arg.c_ty();
                    let computation = write_computation(*input);
                    let computation_ty = write_type(info.computations.get(input).unwrap(), info);

                    quote!({
                        let arg: #computation_ty = __runtime::read(&#computation);
                        __runtime::ToC::<#arg_ty>::to_c(arg)
                    })
                });

                let args = args.iter().map(|arg| {
                    let ty = arg.c_ty();
                    quote!(_: #ty)
                });

                let return_ty = return_ty.map(|ty| ty.c_ty()).unwrap_or_else(|| quote!(()));

                quote!({
                    #[link(name = #lib)]
                    extern "C" {
                        fn #func(#(#args),*) -> #return_ty;
                    }

                    __runtime::FromC::<#return_ty>::from_c(#func(#(#inputs),*))
                })
            }
        }
        ir::ExpressionKind::Structure(values) => {
            let id = match &expr.ty {
                ir::Type::Structure(id, _) => *id,
                _ => unreachable!(),
            };

            let name = write_structure_name(id);

            let fields = values.iter().enumerate().map(|(index, id)| {
                let field = write_structure_field(index);
                let ty = (*info.computations.get(id).unwrap()).clone();

                let value = {
                    let ty = write_type(&ty, info);
                    let id = write_computation(*id);
                    quote!(#field: __runtime::read::<_, #ty>(&#id))
                };

                if matches!(ty, ir::Type::Recursive(_))
                    || matches!(ty, ir::Type::Structure(id, _)
                        | ir::Type::Enumeration(id, _) if is_recursive(id, info))
                {
                    quote!(Box::new(#value))
                } else {
                    value
                }
            });

            quote! {
                #name {
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
                ir::Type::Enumeration(id, _) => *id,
                _ => unreachable!(),
            };

            let name = write_enumeration_name(id);

            let variant = write_enumeration_variant(*discriminant);

            let values = values.iter().map(|id| {
                let ty = (*info.computations.get(id).unwrap()).clone();

                let value = {
                    let ty = write_type(&ty, info);
                    let id = write_computation(*id);
                    quote!(__runtime::read::<_, #ty>(&#id))
                };

                if matches!(ty, ir::Type::Recursive(_))
                    || matches!(ty, ir::Type::Structure(id, _)
                        | ir::Type::Enumeration(id, _) if is_recursive(id, info))
                {
                    quote!(Box::new(#value))
                } else {
                    value
                }
            });

            quote!(#name::#variant(#(#values),*))
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

            let (variants, unbox) = match info.program.nominal_types.get(id).unwrap() {
                ir::Type::Enumeration(_, variants) => (
                    variants[*discriminant].iter().enumerate().map(|(i, _)| {
                        if i == *index {
                            quote!(__element)
                        } else {
                            quote!(_)
                        }
                    }),
                    match variants[*discriminant][*index] {
                        ir::Type::Recursive(_) => Some(quote!(*)),
                        ir::Type::Structure(id, _) | ir::Type::Enumeration(id, _)
                            if is_recursive(id, info) =>
                        {
                            Some(quote!(*))
                        }
                        _ => None,
                    },
                ),
                _ => unreachable!(),
            };

            let name = write_enumeration_name(*id);

            quote! {
                match __runtime::read::<_, #ty>(&#computation) {
                    #name::#variant(#(#variants),*) => #unbox __element,
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

fn write_wrapper(ty: &ir::Type, info: &mut Info) -> TokenStream {
    if let ir::Type::Structure(id, _) | ir::Type::Enumeration(id, _) = ty {
        if is_recursive(*id, info) {
            return quote!(__runtime::BoxedValue);
        }
    }

    quote!(__runtime::Value)
}

fn write_type(ty: &ir::Type, info: &mut Info) -> TokenStream {
    fn write_type(ty: &ir::Type, info: &mut Info, in_field: bool) -> TokenStream {
        match ty {
            ir::Type::Number => quote!(__runtime::Number),
            ir::Type::Integer => quote!(__runtime::Integer),
            ir::Type::Natural => quote!(__runtime::Natural),
            ir::Type::Byte => quote!(__runtime::Byte),
            ir::Type::Signed => quote!(__runtime::Signed),
            ir::Type::Unsigned => quote!(__runtime::Unsigned),
            ir::Type::Float => quote!(__runtime::Float),
            ir::Type::Double => quote!(__runtime::Double),
            ir::Type::Text => quote!(__runtime::Text),
            ir::Type::Discriminant => quote!(__runtime::Discriminant),
            ir::Type::Boolean => quote!(__runtime::Boolean),
            ir::Type::Function(input, output) => {
                let input = write_type(input, info, false);
                let output = write_type(output, info, false);
                quote!(__runtime::Function<#input, #output>)
            }
            ir::Type::Tuple(tys) => {
                let tys = tys.iter().map(|ty| write_type(ty, info, false));
                quote!((#(#tys,)*))
            }
            ir::Type::List(ty) => {
                let ty = write_type(ty, info, false);
                quote!(__runtime::List<#ty>)
            }
            ir::Type::Mutable(ty) => {
                let ty = write_type(ty, info, false);
                quote!(__runtime::Mutable<#ty>)
            }
            ir::Type::Marker => quote!(__runtime::Marker),
            ir::Type::Structure(id, fields) => {
                if !info.types.contains_key(id) {
                    let name = write_structure_name(*id);

                    info.types.insert(*id, (name.clone(), None));

                    let fields = fields.iter().enumerate().map(|(index, ty)| {
                        let field = write_structure_field(index);
                        let ty = write_type(ty, info, false);

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

                write_structure_ty(*id, in_field, info)
            }
            ir::Type::Enumeration(id, variants) => {
                if !info.types.contains_key(id) {
                    let name = write_enumeration_name(*id);

                    info.types.insert(*id, (name.clone(), None));

                    let discriminants = variants.iter().enumerate().map(|(index, _)| {
                        let variant = write_enumeration_variant(index);

                        quote!(#name::#variant(..) => #index,)
                    });

                    let variants = variants.iter().enumerate().map(|(index, tys)| {
                        let variant = write_enumeration_variant(index);
                        let tys = tys.iter().map(|ty| write_type(ty, info, true));

                        quote!(#variant(#(#tys),*),)
                    });

                    let decl = quote! {
                        #[derive(Clone)]
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

                write_enumeration_ty(*id, in_field, info)
            }
            ir::Type::Recursive(id) => {
                let name = info.types.get(id).unwrap().0.clone();

                if in_field {
                    quote!(Box<#name>)
                } else {
                    name
                }
            }
            ir::Type::Unreachable => quote!(__runtime::Unreachable),
        }
    }

    write_type(ty, info, false)
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

fn write_constant(constant: MonomorphizedConstantId) -> TokenStream {
    syn::Ident::new(&format!("__constant_{}", constant.0), Span::call_site()).to_token_stream()
}

fn write_structure_name(id: TypeId) -> TokenStream {
    syn::Ident::new(&format!("__structure_{}", id.0), Span::call_site()).to_token_stream()
}

fn write_structure_ty(id: TypeId, in_field: bool, info: &Info) -> TokenStream {
    let ident = write_structure_name(id);

    if in_field && is_recursive(id, info) {
        quote!(Box<#ident>)
    } else {
        ident.to_token_stream()
    }
}

fn write_structure_field(index: usize) -> TokenStream {
    syn::Ident::new(&format!("__field_{}", index), Span::call_site()).to_token_stream()
}

fn write_enumeration_name(id: TypeId) -> TokenStream {
    syn::Ident::new(&format!("__enumeration_{}", id.0), Span::call_site()).to_token_stream()
}

fn write_enumeration_ty(id: TypeId, in_field: bool, info: &Info) -> TokenStream {
    let ident = write_enumeration_name(id);

    if in_field && is_recursive(id, info) {
        quote!(Box<#ident>)
    } else {
        ident.to_token_stream()
    }
}

fn write_enumeration_variant(index: usize) -> TokenStream {
    syn::Ident::new(&format!("__variant_{}", index), Span::call_site()).to_token_stream()
}

fn is_recursive(id: TypeId, info: &Info) -> bool {
    info.program.recursive_types.contains(&id)
}
