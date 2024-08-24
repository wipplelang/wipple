use crate::{parse, Driver};
use wipple_util::WithInfo;

pub fn attribute<D: Driver>(
    attribute_syntax: WithInfo<D::Info, parse::Attribute<D>>,
) -> WithInfo<D::Info, crate::Attribute<D>> {
    attribute_syntax.map(|attribute_syntax| match attribute_syntax {
        parse::Attribute::Error => crate::Attribute::Error,
        parse::Attribute::Name(name) => crate::Attribute::Name(name),
        parse::Attribute::Valued { name, value } => crate::Attribute::Valued {
            name,
            value: attribute_value(value),
        },
    })
}

fn attribute_value<D: Driver>(
    attribute_value_syntax: WithInfo<D::Info, parse::AttributeValue<D>>,
) -> WithInfo<D::Info, crate::AttributeValue<D>> {
    attribute_value_syntax.map(|attribute_value_syntax| match attribute_value_syntax {
        parse::AttributeValue::Error => crate::AttributeValue::Error,
        parse::AttributeValue::Name(name) => crate::AttributeValue::Name(name),
        parse::AttributeValue::Text(text) => crate::AttributeValue::Text(text),
        parse::AttributeValue::Number(number) => crate::AttributeValue::Number(number),
    })
}
