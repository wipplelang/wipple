use crate::{resolve::Info, Driver};
use wipple_util::WithInfo;

pub fn check_parameter_count<D: Driver>(
    declaration: D::Info,
    expected_count: u32,
    parameters: &[WithInfo<D::Info, crate::UnresolvedType<D>>],
    info: &mut Info<D>,
) {
    match (parameters.len() as u32).cmp(&expected_count) {
        std::cmp::Ordering::Equal => {}
        std::cmp::Ordering::Less => {
            info.errors.push(WithInfo {
                info: declaration,
                item: crate::Diagnostic::MissingTypes(expected_count - parameters.len() as u32),
            });
        }
        std::cmp::Ordering::Greater => {
            for parameter in parameters.iter().skip(expected_count as usize) {
                info.errors
                    .push(parameter.replace(crate::Diagnostic::ExtraType));
            }
        }
    }
}
