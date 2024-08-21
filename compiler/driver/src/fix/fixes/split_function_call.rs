use crate::{
    fix::{fixes::Rule, Fix},
    visit::find_expression_with_info,
    Diagnostic, Info, Interface, Library,
};
use wipple_typecheck::{TypedExpression, TypedExpressionKind};
use wipple_util::WithInfo;

/// A rule that attempts to fix a function call that's missing inputs because
/// the inputs are on the next line, by joining the lines together.
pub(super) struct SplitFunctionCallRule;

impl Rule for SplitFunctionCallRule {
    fn apply(
        &self,
        diagnostic: WithInfo<Info, &Diagnostic>,
        _interface: &Interface,
        library: &Library,
    ) -> Option<Fix> {
        if !matches!(
            diagnostic.item,
            Diagnostic::Typecheck(wipple_typecheck::Diagnostic::MissingInputs(_))
        ) {
            return None;
        }

        let (expression, parent) = library.items.values().find_map(|item| {
            find_expression_with_info(item.expression.as_ref(), &diagnostic.info)
        })?;

        // This fix only applies to function calls in statement position
        if !matches!(
            parent.map(|expression| expression.item.kind),
            Some(TypedExpressionKind::Block { .. } | TypedExpressionKind::Initialize { .. })
        ) {
            return None;
        }

        // Invalid function calls are wrapped in `Unknown`...
        let expression = match expression.item.kind {
            TypedExpressionKind::Unknown(original) => WithInfo {
                info: expression.info,
                item: TypedExpression {
                    r#type: expression.item.r#type,
                    kind: *original?,
                },
            },
            // ...but we also handle functions that are not wrapped in a call
            // expression
            _ => expression,
        };

        let (function, inputs) = match expression.item.kind {
            TypedExpressionKind::Call { function, inputs } => (function.unboxed(), inputs),
            TypedExpressionKind::Constant { .. }
            | TypedExpressionKind::Trait { .. }
            | TypedExpressionKind::Variant { .. } => (expression, Vec::new()),
            _ => return None,
        };

        let input_types = match function.item.r#type {
            wipple_typecheck::Type::Function { inputs, .. } => inputs,
            _ => return None,
        };

        if inputs.len() >= input_types.len() {
            return None;
        }

        Some(Fix::JoinWithNextLine)
    }

    fn succeeded(
        &self,
        fix: wipple_util::WithInfo<Info, &Fix>,
        _interface: &Interface,
        library: &Library,
    ) -> bool {
        let expression = match library
            .items
            .values()
            .find_map(|item| find_expression_with_info(item.expression.as_ref(), &fix.info))
        {
            Some((expression, _)) => expression,
            _ => return false,
        };

        let (function, inputs) = match expression.item.kind {
            TypedExpressionKind::Call { function, inputs } => (function.unboxed(), inputs),
            _ => return false,
        };

        let input_types = match function.item.r#type {
            wipple_typecheck::Type::Function { inputs, .. } => inputs,
            _ => return false,
        };

        // The fix succeeded if we have the correct number of inputs and they
        // have the correct types
        inputs.len() == input_types.len()
            && input_types
                .iter()
                .zip(inputs.iter())
                .all(|(input_type, input)| input_type.item.could_unify_with(&input.item.r#type))
    }
}
