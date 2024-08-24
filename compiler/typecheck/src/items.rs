use crate::{utils::resolve_trait_type_from_instance, Driver};
use wipple_util::WithInfo;

pub struct ItemDeclarationInner<D: Driver> {
    pub(crate) bounds: Vec<WithInfo<D::Info, crate::Instance<D>>>,
    pub(crate) r#type: WithInfo<D::Info, crate::Type<D>>,
    pub(crate) body: WithInfo<D::Info, crate::UntypedExpression<D>>,
    pub(crate) captures: Vec<D::Path>,
}

impl<D: Driver> crate::IntoItemDeclaration<D>
    for (
        WithInfo<D::Info, crate::ConstantDeclaration<D>>,
        crate::UntypedItem<D>,
    )
{
    fn into_item_declaration(
        self,
        _driver: &D,
        _errors: &mut Vec<WithInfo<D::Info, crate::Diagnostic<D>>>,
    ) -> WithInfo<D::Info, Option<crate::ItemDeclaration<D>>> {
        let (declaration, item) = self;

        declaration.map(|declaration| {
            Some(crate::ItemDeclaration(ItemDeclarationInner {
                bounds: declaration.bounds,
                r#type: declaration.r#type,
                body: item.body,
                captures: item.captures,
            }))
        })
    }
}

impl<D: Driver> crate::IntoItemDeclaration<D>
    for (
        WithInfo<D::Info, crate::InstanceDeclaration<D>>,
        Option<crate::UntypedItem<D>>,
    )
{
    fn into_item_declaration(
        self,
        driver: &D,
        errors: &mut Vec<WithInfo<D::Info, crate::Diagnostic<D>>>,
    ) -> WithInfo<D::Info, Option<crate::ItemDeclaration<D>>> {
        let (declaration, item) = self;

        let info = declaration.info.clone();

        let r#type = resolve_trait_type_from_instance(driver, declaration.item.instance.as_ref());

        WithInfo {
            info: info.clone(),
            item: match (r#type, item) {
                (Some(r#type), Some(item)) => Some(crate::ItemDeclaration(ItemDeclarationInner {
                    bounds: declaration.item.bounds,
                    r#type,
                    body: item.body,
                    captures: item.captures,
                })),
                (Some(r#type), None) => {
                    errors.push(WithInfo {
                        info,
                        item: crate::Diagnostic::ExpectedInstanceValue,
                    });

                    Some(crate::ItemDeclaration(ItemDeclarationInner {
                        bounds: declaration.item.bounds,
                        r#type,
                        body: WithInfo {
                            info: declaration.info,
                            item: crate::UntypedExpression::Unknown,
                        },
                        captures: Vec::new(),
                    }))
                }
                (None, Some(_)) => {
                    errors.push(WithInfo {
                        info,
                        item: crate::Diagnostic::UnexpectedInstanceValue,
                    });

                    None
                }
                (None, None) => None,
            },
        }
    }
}

impl<D: Driver> crate::IntoItemDeclaration<D> for crate::UntypedTopLevelCode<D> {
    fn into_item_declaration(
        self,
        driver: &D,
        _errors: &mut Vec<WithInfo<D::Info, crate::Diagnostic<D>>>,
    ) -> WithInfo<D::Info, Option<crate::ItemDeclaration<D>>> {
        WithInfo {
            info: driver.top_level_info(),
            item: Some(crate::ItemDeclaration(ItemDeclarationInner {
                bounds: Vec::new(),
                r#type: WithInfo {
                    info: driver.top_level_info(),
                    item: crate::Type::Unknown, // the top level can be any type
                },
                body: WithInfo {
                    info: driver.top_level_info(),
                    item: crate::UntypedExpression::Block {
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
