use crate::{
    lower::{expression::resolve_expression, language::resolve_language_item, resolve::Info},
    util::WithInfo,
};

pub fn resolve_binary_operator(
    operator: WithInfo<crate::lower::UnresolvedBinaryOperator>,
    left: WithInfo<crate::lower::UnresolvedExpression>,
    right: WithInfo<crate::lower::UnresolvedExpression>,
    info: &mut Info,
) -> crate::lower::Expression {
    let operator_trait = operator.replace(
        match resolve_language_item(operator.as_ref().map(ToString::to_string), info)
            .into_iter()
            .find(|path| matches!(path.last().unwrap(), crate::lower::PathComponent::Trait(_)))
        {
            Some(mut path) => {
                let name = match path.last().unwrap() {
                    crate::lower::PathComponent::Trait(name) => name.clone(),
                    _ => unreachable!(),
                };

                path.push(crate::lower::PathComponent::Constructor(name));

                crate::lower::Expression::Constant(path)
            }
            None => crate::lower::Expression::Error,
        },
    );

    let left = resolve_expression(left, info);

    let right = if operator.item.short_circuits() {
        let right_info = right.info.clone();
        resolve_expression(
            right.map(|right| {
                crate::lower::UnresolvedExpression::Block(vec![WithInfo {
                    info: right_info.clone(),
                    item: crate::lower::UnresolvedStatement::Expression(WithInfo {
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

    crate::lower::Expression::Call {
        function: operator_trait.boxed(),
        inputs: vec![left, right],
    }
}
