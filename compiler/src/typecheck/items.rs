use crate::{
    lower::Path,
    typecheck::{Driver, utils::resolve_trait_type_from_instance},
    util::WithInfo,
};

pub struct ItemDeclarationInner {
    pub(crate) bounds: Vec<WithInfo<crate::typecheck::Instance>>,
    pub(crate) r#type: WithInfo<crate::typecheck::Type>,
    pub(crate) body: WithInfo<crate::typecheck::UntypedExpression>,
    pub(crate) captures: Vec<Path>,
}

impl crate::typecheck::IntoItemDeclaration
    for (
        WithInfo<crate::typecheck::ConstantDeclaration>,
        crate::typecheck::UntypedItem,
    )
{
    fn into_item_declaration(
        self,
        _driver: &dyn Driver,
        _errors: &mut Vec<WithInfo<crate::typecheck::Diagnostic>>,
    ) -> WithInfo<Option<crate::typecheck::ItemDeclaration>> {
        let (declaration, item) = self;

        declaration.map(|declaration| {
            Some(crate::typecheck::ItemDeclaration(ItemDeclarationInner {
                bounds: declaration.bounds,
                r#type: declaration.r#type,
                body: item.body,
                captures: item.captures,
            }))
        })
    }
}

impl crate::typecheck::IntoItemDeclaration
    for (
        WithInfo<crate::typecheck::InstanceDeclaration>,
        Option<crate::typecheck::UntypedItem>,
    )
{
    fn into_item_declaration(
        self,
        driver: &dyn Driver,
        errors: &mut Vec<WithInfo<crate::typecheck::Diagnostic>>,
    ) -> WithInfo<Option<crate::typecheck::ItemDeclaration>> {
        let (declaration, item) = self;

        let info = declaration.info.clone();

        let r#type = resolve_trait_type_from_instance(driver, declaration.item.instance.as_ref());

        WithInfo {
            info: info.clone(),
            item: match (r#type, item) {
                (Some(r#type), Some(item)) => {
                    Some(crate::typecheck::ItemDeclaration(ItemDeclarationInner {
                        bounds: declaration.item.bounds,
                        r#type,
                        body: item.body,
                        captures: item.captures,
                    }))
                }
                (Some(r#type), None) => {
                    errors.push(WithInfo {
                        info,
                        item: crate::typecheck::Diagnostic::ExpectedInstanceValue,
                    });

                    Some(crate::typecheck::ItemDeclaration(ItemDeclarationInner {
                        bounds: declaration.item.bounds,
                        r#type,
                        body: WithInfo {
                            info: declaration.info,
                            item: crate::typecheck::UntypedExpression::Unknown,
                        },
                        captures: Vec::new(),
                    }))
                }
                (None, Some(_)) => {
                    errors.push(WithInfo {
                        info,
                        item: crate::typecheck::Diagnostic::UnexpectedInstanceValue,
                    });

                    None
                }
                (None, None) => None,
            },
        }
    }
}

impl crate::typecheck::IntoItemDeclaration for crate::typecheck::UntypedTopLevelCode {
    fn into_item_declaration(
        self,
        driver: &dyn Driver,
        _errors: &mut Vec<WithInfo<crate::typecheck::Diagnostic>>,
    ) -> WithInfo<Option<crate::typecheck::ItemDeclaration>> {
        WithInfo {
            info: driver.top_level_info(),
            item: Some(crate::typecheck::ItemDeclaration(ItemDeclarationInner {
                bounds: Vec::new(),
                r#type: WithInfo {
                    info: driver.top_level_info(),
                    item: crate::typecheck::Type::Unknown, // the top level can be any type
                },
                body: WithInfo {
                    info: driver.top_level_info(),
                    item: crate::typecheck::UntypedExpression::Block {
                        statements: self.statements,
                        top_level: true,
                        captures: Vec::new(),
                    },
                },
                captures: Vec::new(),
            })),
        }
    }
}
