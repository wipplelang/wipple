use crate::{
    expression::resolve_expression, language::resolve_language_item, resolve::Info, Driver,
};
use wipple_util::WithInfo;

pub fn resolve_binary_operator<D: Driver>(
    operator: WithInfo<D::Info, crate::UnresolvedBinaryOperator>,
    left: WithInfo<D::Info, crate::UnresolvedExpression<D>>,
    right: WithInfo<D::Info, crate::UnresolvedExpression<D>>,
    info: &mut Info<D>,
) -> crate::Expression<D> {
    let operator_trait = operator.replace(
        match resolve_language_item(operator.as_ref().map(ToString::to_string), info)
            .into_iter()
            .find(|path| matches!(path.last().unwrap(), crate::PathComponent::Trait(_)))
        {
            Some(mut path) => {
                let name = match path.last().unwrap() {
                    crate::PathComponent::Trait(name) => name.clone(),
                    _ => unreachable!(),
                };

                path.push(crate::PathComponent::Constructor(name));

                crate::Expression::Constant(path)
            }
            None => crate::Expression::Error,
        },
    );

    let left = resolve_expression(left, info);

    let right = if operator.item.short_circuits() {
        let right_info = right.info.clone();
        resolve_expression(
            right.map(|right| {
                crate::UnresolvedExpression::Block(vec![WithInfo {
                    info: right_info.clone(),
                    item: crate::UnresolvedStatement::Expression(WithInfo {
                        info: right_info,
                        item: right,
                    }),
                }])
            }),
            info,
        )
    } else {
        resolve_expression(right, info)
    };

    crate::Expression::Call {
        function: operator_trait.boxed(),
        inputs: vec![left, right],
    }
}
