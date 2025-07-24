use crate::{
    syntax::{
        ast::{Info, expression::expression, r#type::r#type},
        parse,
    },
    util::WithInfo,
};

pub fn pattern(
    pattern_syntax: WithInfo<parse::Pattern>,
    info: &mut Info,
) -> WithInfo<crate::syntax::Pattern> {
    pattern_syntax.map(|pattern_syntax| match pattern_syntax {
        parse::Pattern::Error => crate::syntax::Pattern::Error,
        parse::Pattern::Wildcard => crate::syntax::Pattern::Wildcard,
        parse::Pattern::Number(num) => crate::syntax::Pattern::Number(num),
        parse::Pattern::Text(text) => crate::syntax::Pattern::Text(text),
        parse::Pattern::Name(name) => crate::syntax::Pattern::Name(name),
        parse::Pattern::VariantOrName(variant) => crate::syntax::Pattern::VariantOrName(variant),
        parse::Pattern::Destructure(field_syntaxes) => crate::syntax::Pattern::Destructure(
            field_syntaxes
                .into_iter()
                .map(|field_pattern_syntax| field_pattern(field_pattern_syntax, info))
                .collect(),
        ),
        parse::Pattern::Variant {
            variant,
            value_patterns: value_syntaxes,
        } => crate::syntax::Pattern::Variant {
            variant,
            value_patterns: value_syntaxes
                .into_iter()
                .map(|pattern_syntax| pattern(pattern_syntax, info))
                .collect(),
        },
        parse::Pattern::Tuple(pattern_syntaxes) => crate::syntax::Pattern::Tuple(
            pattern_syntaxes
                .into_iter()
                .map(|pattern_syntax| pattern(pattern_syntax, info))
                .collect(),
        ),
        parse::Pattern::Or {
            left: left_syntax,
            right: right_syntax,
        } => crate::syntax::Pattern::Or {
            left: pattern(left_syntax.unboxed(), info).boxed(),
            right: pattern(right_syntax.unboxed(), info).boxed(),
        },
        parse::Pattern::Mutate(name) => crate::syntax::Pattern::Mutate(name),
        parse::Pattern::Annotate {
            pattern: pattern_syntax,
            r#type: type_syntax,
        } => crate::syntax::Pattern::Annotate {
            pattern: pattern(pattern_syntax.unboxed(), info).boxed(),
            r#type: r#type(type_syntax, info),
        },
    })
}

pub fn field_pattern(
    field_pattern_syntax: WithInfo<parse::FieldPattern>,
    info: &mut Info,
) -> WithInfo<crate::syntax::FieldPattern> {
    field_pattern_syntax.map(|field_pattern_syntax| crate::syntax::FieldPattern {
        name: field_pattern_syntax.name,
        pattern: pattern(field_pattern_syntax.pattern, info),
    })
}

pub fn arm(arm_syntax: WithInfo<parse::Arm>, info: &mut Info) -> WithInfo<crate::syntax::Arm> {
    arm_syntax.map(|arm_syntax| crate::syntax::Arm {
        pattern: pattern(arm_syntax.pattern, info),
        body: expression(arm_syntax.body, info),
    })
}
