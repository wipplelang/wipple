use crate::{lower::resolve::Info, util::WithInfo};

pub fn resolve_language_item(name: WithInfo<String>, info: &mut Info) -> Vec<crate::lower::Path> {
    match info
        .language_declarations
        .get(&name.item)
        .or_else(|| info.existing.language_declarations.get(&name.item))
    {
        Some(paths) => paths.clone(),
        None => {
            info.errors
                .push(name.map(crate::lower::Diagnostic::UnresolvedLanguageItem));

            Vec::new()
        }
    }
}
