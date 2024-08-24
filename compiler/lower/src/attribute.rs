use crate::{resolve::Info, Driver};
use wipple_util::WithInfo;

pub fn resolve_attributes<D: Driver>(
    attributes: &[WithInfo<D::Info, crate::Attribute<D>>],
    info: &mut Info<D>,
) {
    for attribute in attributes {
        if let crate::Attribute::Valued { name, value } = &attribute.item {
            if name.item.as_str() == "language" {
                if let crate::AttributeValue::Text(language_item) = &value.item {
                    info.language_declarations
                        .entry(language_item.item.clone())
                        .or_default()
                        .push(info.path.clone());
                }
            }
        }
    }
}
