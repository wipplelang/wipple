use crate::{resolve::Info, Driver};
use wipple_util::WithInfo;

pub fn resolve_language_item<D: Driver>(
    name: WithInfo<D::Info, String>,
    info: &mut Info<D>,
) -> Vec<crate::Path> {
    match info
        .language_declarations
        .get(&name.item)
        .or_else(|| info.dependencies.language_declarations.get(&name.item))
    {
        Some(paths) => paths.clone(),
        None => {
            info.errors
                .push(name.map(crate::Diagnostic::UnresolvedLanguageItem));

            Vec::new()
        }
    }
}
