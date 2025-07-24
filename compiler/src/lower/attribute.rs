use crate::{lower::resolve::Info, util::WithInfo};

pub fn resolve_attributes(attributes: &[WithInfo<crate::lower::Attribute>], info: &mut Info) {
    for attribute in attributes {
        if let crate::lower::Attribute::Valued { name, value } = &attribute.item {
            if name.item.as_str() == "language" {
                if let crate::lower::AttributeValue::Text(language_item) = &value.item {
                    info.language_declarations
                        .entry(language_item.item.clone())
                        .or_default()
                        .push(info.path.clone());
                }
            }
        }
    }
}
