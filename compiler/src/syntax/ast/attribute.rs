use crate::{syntax::parse, util::WithInfo};

pub fn attribute(
    attribute_syntax: WithInfo<parse::Attribute>,
) -> WithInfo<crate::syntax::Attribute> {
    attribute_syntax.map(|attribute_syntax| match attribute_syntax {
        parse::Attribute::Error => crate::syntax::Attribute::Error,
        parse::Attribute::Name(name) => crate::syntax::Attribute::Name(name),
        parse::Attribute::Valued { name, value } => crate::syntax::Attribute::Valued {
            name,
            value: attribute_value(value),
        },
    })
}

fn attribute_value(
    attribute_value_syntax: WithInfo<parse::AttributeValue>,
) -> WithInfo<crate::syntax::AttributeValue> {
    attribute_value_syntax.map(|attribute_value_syntax| match attribute_value_syntax {
        parse::AttributeValue::Error => crate::syntax::AttributeValue::Error,
        parse::AttributeValue::Name(name) => crate::syntax::AttributeValue::Name(name),
        parse::AttributeValue::Text(text) => crate::syntax::AttributeValue::Text(text),
        parse::AttributeValue::Number(number) => crate::syntax::AttributeValue::Number(number),
    })
}
