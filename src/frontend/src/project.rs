use crate::lower::*;
use std::{
    fs, io,
    path::{Path, PathBuf},
    rc::Rc,
    sync::Arc,
};
use url::Url;
use wipple_diagnostics::*;
use wipple_parser::LocalIntern;

#[derive(Default)]
pub struct Project {
    pub base: Option<Base>,
    pub cache_path: Option<PathBuf>,
}

pub enum Base {
    Url(Url),
    Path(PathBuf),
}

pub fn load_file(name: &str, span: Span, info: &mut Info) -> Option<Rc<File>> {
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

            match load_path(path.to_str().unwrap(), path, info) {
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

pub fn load_url(name: &str, url: &Url, info: &mut Info) -> io::Result<Option<Rc<File>>> {
    todo!()
}

pub fn load_path(name: &str, path: &Path, info: &mut Info) -> io::Result<Option<Rc<File>>> {
    let code = Arc::from(fs::read_to_string(path)?);
    Ok(load_string(name, code, info))
}

pub fn load_string(name: &str, code: Arc<str>, info: &mut Info) -> Option<Rc<File>> {
    info.diagnostics
        .add_file(LocalIntern::from(name), Arc::clone(&code));

    wipple_parser::parse(LocalIntern::from(name), &code, info.diagnostics)
        .and_then(|file| lower(file, info))
}
