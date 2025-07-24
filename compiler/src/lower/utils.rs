use crate::{lower::resolve::Info, syntax::Location, util::WithInfo};

pub fn check_parameter_count(
    declaration: Location,
    expected_count: u32,
    parameters: &[WithInfo<crate::lower::UnresolvedType>],
    info: &mut Info,
) {
    match (parameters.len() as u32).cmp(&expected_count) {
        std::cmp::Ordering::Equal => {}
        std::cmp::Ordering::Less => {
            info.errors.push(WithInfo {
                info: declaration,
                item: crate::lower::Diagnostic::MissingTypes(
                    expected_count - parameters.len() as u32,
                ),
            });
        }
        std::cmp::Ordering::Greater => {
            for parameter in parameters.iter().skip(expected_count as usize) {
                info.errors
                    .push(parameter.replace(crate::lower::Diagnostic::ExtraType));
            }
        }
    }
}
