use crate::{compile::*, *};
use std::{
    fs,
    path::{Path, PathBuf},
};
use url::Url;
use wipple_diagnostics::*;

#[derive(Default)]
pub struct Project {
    pub base: Option<Base>,
    pub cache_path: Option<PathBuf>,
}

pub enum Base {
    Url(Url),
    Path(PathBuf),
}

pub fn load_file(name: &str, span: Span, info: &mut Info) -> Option<Arc<File>> {
    macro_rules! error {
        ($msg:expr, $error:expr) => {{
            info.diagnostics.add(Diagnostic::new(
                DiagnosticLevel::Error,
                $msg,
                vec![Note::primary(span, $error)],
            ));

            return None;
        }};
    }

    macro_rules! load_from_url {
        ($url:expr) => {{
            let url = $url;

            match load_url(url.as_str(), url, info) {
                Ok(file) => file,
                Err(error) => error!("Could not load file", error),
            }
        }};
    }

    macro_rules! load_from_path {
        ($path:expr) => {{
            let path = $path;

            match path
                .canonicalize()
                .map_err(anyhow::Error::msg)
                .and_then(|path| load_path(path.to_str().unwrap(), &path, info))
            {
                Ok(file) => file,
                Err(error) => error!("Could not load file", error),
            }
        }};
    }

    match Url::parse(name) {
        Ok(url) => load_from_url!(&url),
        Err(error) => {
            if error == url::ParseError::RelativeUrlWithoutBase {
                let path = match name.parse::<PathBuf>() {
                    Ok(path) => path,
                    Err(error) => error!("Invalid path", error),
                };

                if path.is_absolute() {
                    load_from_path!(&path)
                } else {
                    match &info.project.base {
                        Some(Base::Path(base)) => load_from_path!(&base.join(path)),
                        Some(Base::Url(url)) => match url.join(path.to_str().unwrap()) {
                            Ok(url) => load_from_url!(&url),
                            Err(error) => error!("Invalid URL", error),
                        },
                        None => error!(
                            "Unexpected relative path",
                            "Cannot use relative path outside project"
                        ),
                    }
                }
            } else {
                error!("Invalid URL", error)
            }
        }
    }
}

pub fn load_url(name: &str, url: &Url, info: &mut Info) -> anyhow::Result<Option<Arc<File>>> {
    #[cfg(target_arch = "wasm32")]
    {
        let _ = (name, url, info);

        return Err(anyhow::Error::msg(
            "Loading from URLs is not supported in the playground",
        ));
    }

    #[cfg(not(target_arch = "wasm32"))]
    {
        let response = reqwest::blocking::get(url.to_string())?;
        let code = response.text()?;
        Ok(load_string(name, code.into(), info))
    }
}

pub fn load_path(name: &str, path: &Path, info: &mut Info) -> anyhow::Result<Option<Arc<File>>> {
    let code = fs::read_to_string(path)?;
    Ok(load_string(name, code.into(), info))
}

pub fn load_string(name: &str, code: Arc<str>, info: &mut Info) -> Option<Arc<File>> {
    info.files
        .iter()
        .find(|file| file.name.as_str() == name)
        .cloned()
        .or_else(|| {
            let name = InternedString::new(name);
            info.diagnostics.add_file(name, Arc::clone(&code));
            wipple_parser::parse(name, &code, info.diagnostics).and_then(|file| lower(file, info))
        })
}
