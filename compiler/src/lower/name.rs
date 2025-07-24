use crate::{lower::resolve::Info, util::WithInfo};

pub fn resolve_name<T>(
    name: WithInfo<String>,
    info: &mut Info,
    filter: impl FnMut(&[WithInfo<crate::lower::Path>]) -> Vec<(crate::lower::Path, T)>,
) -> Option<T> {
    let result = try_resolve_name(name.clone(), info, filter);

    if result.is_none() {
        info.errors
            .push(name.map(crate::lower::Diagnostic::UnresolvedName));
    }

    result
}

pub fn try_resolve_name<T>(
    name: WithInfo<String>,
    info: &mut Info,
    mut filter: impl FnMut(&[WithInfo<crate::lower::Path>]) -> Vec<(crate::lower::Path, T)>,
) -> Option<T> {
    let mut allow_locals = true;
    for scope in info.scopes.0.iter().rev() {
        if let Some(paths) = scope.paths.get(&name.item) {
            let paths = paths
                .iter()
                .filter(|path| allow_locals || !path.item.last().unwrap().is_local())
                .cloned()
                .collect::<Vec<_>>();

            let mut candidates = filter(&paths);

            match candidates.len() {
                0 => {
                    if scope.filters_locals() {
                        allow_locals = false;
                    }

                    continue;
                }
                1 => {
                    let (path, candidate) = candidates.pop().unwrap();
                    info.capture_if_variable(&path);

                    return Some(candidate);
                }
                _ => {
                    info.errors
                        .push(name.replace(crate::lower::Diagnostic::AmbiguousName {
                            name: name.item.clone(),
                            candidates: paths.iter().map(|path| path.item.clone()).collect(),
                        }));

                    // Try the last candidate defined
                    let (path, candidate) = candidates.pop().unwrap();
                    info.capture_if_variable(&path);

                    return Some(candidate);
                }
            }
        } else if scope.filters_locals() {
            allow_locals = false;
        }
    }

    None
}
