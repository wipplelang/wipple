use crate::{
    ast::{expression::expression, r#type::r#type, Info},
    parse, Driver,
};
use wipple_util::WithInfo;

pub fn pattern<D: Driver>(
    pattern_syntax: WithInfo<D::Info, parse::Pattern<D>>,
    info: &mut Info<D>,
) -> WithInfo<D::Info, crate::Pattern<D>> {
    pattern_syntax.map(|pattern_syntax| match pattern_syntax {
        parse::Pattern::Error => crate::Pattern::Error,
        parse::Pattern::Wildcard => crate::Pattern::Wildcard,
        parse::Pattern::Unit => crate::Pattern::Tuple(Vec::new()),
        parse::Pattern::Number(num) => crate::Pattern::Number(num),
        parse::Pattern::Text(text) => crate::Pattern::Text(text),
        parse::Pattern::Name(name) => crate::Pattern::Name(name),
        parse::Pattern::VariantOrName(variant) => crate::Pattern::VariantOrName(variant),
        parse::Pattern::Destructure(field_syntaxes) => crate::Pattern::Destructure(
            field_syntaxes
                .into_iter()
                .map(|field_pattern_syntax| field_pattern(field_pattern_syntax, info))
                .collect(),
        ),
        parse::Pattern::Variant {
            variant,
            value_patterns: value_syntaxes,
        } => crate::Pattern::Variant {
            variant,
            value_patterns: value_syntaxes
                .into_iter()
                .map(|pattern_syntax| pattern(pattern_syntax, info))
                .collect(),
        },
        parse::Pattern::Tuple(pattern_syntaxes) => crate::Pattern::Tuple(
            pattern_syntaxes
                .into_iter()
                .map(|pattern_syntax| pattern(pattern_syntax, info))
                .collect(),
        ),
        parse::Pattern::Or {
            left: left_syntax,
            right: right_syntax,
        } => crate::Pattern::Or {
            left: pattern(left_syntax.unboxed(), info).boxed(),
            right: pattern(right_syntax.unboxed(), info).boxed(),
        },
        parse::Pattern::Mutate(name) => crate::Pattern::Mutate(name),
        parse::Pattern::Annotate {
            pattern: pattern_syntax,
            r#type: type_syntax,
        } => crate::Pattern::Annotate {
            pattern: pattern(pattern_syntax.unboxed(), info).boxed(),
            r#type: r#type(type_syntax, info),
        },
    })
}

pub fn field_pattern<D: Driver>(
    field_pattern_syntax: WithInfo<D::Info, parse::FieldPattern<D>>,
    info: &mut Info<D>,
) -> WithInfo<D::Info, crate::FieldPattern<D>> {
    field_pattern_syntax.map(|field_pattern_syntax| crate::FieldPattern {
        name: field_pattern_syntax.name,
        pattern: pattern(field_pattern_syntax.pattern, info),
    })
}

pub fn arm<D: Driver>(
    arm_syntax: WithInfo<D::Info, parse::Arm<D>>,
    info: &mut Info<D>,
) -> WithInfo<D::Info, crate::Arm<D>> {
    arm_syntax.map(|arm_syntax| crate::Arm {
        pattern: pattern(arm_syntax.pattern, info),
        body: expression(arm_syntax.body, info),
    })
}
