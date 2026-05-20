use crate::{
    db::{Db, Fact, Node},
    facts::Syntax,
    render::{Render, RenderCtx},
    span::Span,
    visit::Visit,
};
use arcstr::Substr;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExtraAttributeValue;

#[typetag::serde]
impl Fact for ExtraAttributeValue {}

impl Render for ExtraAttributeValue {
    fn render_into(&self, _db: &Db, ctx: &mut RenderCtx) {
        ctx.string("extra attribute value");
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DuplicateAttribute;

#[typetag::serde]
impl Fact for DuplicateAttribute {}

impl Render for DuplicateAttribute {
    fn render_into(&self, _db: &Db, ctx: &mut RenderCtx) {
        ctx.string("duplicate attribute");
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MismatchedAttributeValue;

#[typetag::serde]
impl Fact for MismatchedAttributeValue {}

impl Render for MismatchedAttributeValue {
    fn render_into(&self, _db: &Db, ctx: &mut RenderCtx) {
        ctx.string("mismatched attribute value");
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MissingAttributeValue;

#[typetag::serde]
impl Fact for MissingAttributeValue {}

impl Render for MissingAttributeValue {
    fn render_into(&self, _db: &Db, ctx: &mut RenderCtx) {
        ctx.string("missing attribute value");
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Attribute {
    pub span: Span,
    pub name: Substr,
    pub value: Option<Box<dyn Visit>>,
}

#[typetag::serde]
impl Visit for Attribute {
    fn span(&self) -> &Span {
        &self.span
    }

    fn is_hidden(&self) -> bool {
        true
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StringAttributeValue {
    pub span: Span,
    pub value: Substr,
}

#[typetag::serde]
impl Visit for StringAttributeValue {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConnectionAttributeValue {
    pub span: Span,
    pub left: Substr,
    pub right: Substr,
    pub label: Substr,
}

#[typetag::serde]
impl Visit for ConnectionAttributeValue {
    fn span(&self) -> &Span {
        &self.span
    }
}

pub fn parse_attribute_named(db: &mut Db, attributes: &[Node], name: &str) -> bool {
    let mut found = false;

    for &node in attributes {
        let Some(attribute) = db
            .get(node)
            .and_then(|Syntax(syntax)| syntax.downcast_ref::<Attribute>())
        else {
            continue;
        };

        if attribute.name == name {
            if attribute.value.is_some() {
                db.insert(node, ExtraAttributeValue);
            } else if found {
                db.insert(node, DuplicateAttribute);
            } else {
                found = true;
            }
        }
    }

    found
}

pub fn parse_attribute_with_value<T: Visit + Clone>(
    db: &mut Db,
    attributes: &[Node],
    name: &str,
) -> Option<T> {
    let mut result = None;
    for &node in attributes {
        let Some(attribute) = db
            .get(node)
            .and_then(|Syntax(syntax)| syntax.downcast_ref::<Attribute>())
        else {
            continue;
        };

        if attribute.name == name {
            if let Some(value) = &attribute.value {
                if result.is_some() {
                    db.insert(node, ExtraAttributeValue);
                    continue;
                }

                result = value.as_ref().downcast_ref::<T>().cloned();

                if result.is_none() {
                    db.insert(node, MismatchedAttributeValue);
                }
            } else {
                db.insert(node, MissingAttributeValue);
            }
        }
    }

    result
}

pub fn parse_attributes_with_value<T: Visit + Clone>(
    db: &mut Db,
    attributes: &[Node],
    name: &str,
) -> Vec<T> {
    attributes
        .iter()
        .filter_map(|&node| {
            let attribute = db
                .get(node)
                .and_then(|Syntax(syntax)| syntax.downcast_ref::<Attribute>())?;

            if attribute.name != name {
                return None;
            }

            let Some(result) = attribute.value.as_ref()?.downcast_ref::<T>() else {
                db.insert(node, MismatchedAttributeValue);
                return None;
            };

            Some(result.clone())
        })
        .collect()
}
