#![no_main]

use async_trait::async_trait;
use futures::future::BoxFuture;
use libfuzzer_sys::fuzz_target;
use std::{collections::HashMap, sync::Arc};
use wipple_syntax::{ast::Format, DriverExt};

#[derive(Debug, Clone)]
enum Driver {
    Code(String),
    Fuzz(Arc<wipple_syntax::ast::File<Self>>),
}

#[async_trait]
impl wipple_syntax::Driver for Driver {
    type InternedString = wipple_syntax::FuzzString;
    type Path = ();
    type Span = ();
    type File = wipple_syntax::SingleFile;
    type Scope = ();

    fn intern(&self, s: impl AsRef<str>) -> Self::InternedString {
        wipple_syntax::FuzzString(s.as_ref().to_string())
    }

    fn make_path(&self, _path: Self::InternedString) -> Option<Self::Path> {
        None
    }

    fn make_span(&self, _path: Self::Path, _range: std::ops::Range<usize>) -> Self::Span {
        ()
    }

    fn std_path(&self) -> Option<Self::Path> {
        None
    }

    async fn load_file(
        &self,
        _source_file: Option<(Self::Path, Self::File)>,
        _source_span: Option<Self::Span>,
        _path: Self::Path,
        expand: impl FnOnce(
                Self::Path,
                Self::File,
            ) -> BoxFuture<'static, Arc<wipple_syntax::ast::File<Self>>>
            + Send
            + 'static,
    ) -> Option<Arc<wipple_syntax::ast::File<Self>>> {
        Some(match self {
            Driver::Code(code) => expand((), wipple_syntax::SingleFile(code.clone())).await,
            Driver::Fuzz(fuzz) => fuzz.clone(),
        })
    }

    fn syntax_error_with(&self, _msgs: impl IntoIterator<Item = (Self::Span, String)>) {
        // do nothing
    }

    fn backtrace(&self) -> wipple_util::Backtrace {
        wipple_util::Backtrace::empty()
    }
}

impl wipple_syntax::FuzzDriver for Driver {}

#[derive(Debug, Clone, Default)]
struct Loader {
    code: String,
    virtual_paths: wipple_util::Shared<HashMap<wipple_frontend::helpers::InternedString, Arc<str>>>,
    cache: wipple_util::Shared<
        HashMap<
            wipple_frontend::FilePath,
            Arc<wipple_frontend::analysis::ast::File<wipple_frontend::analysis::Analysis>>,
        >,
    >,
    source_map: wipple_util::Shared<wipple_frontend::SourceMap>,
}

#[async_trait]
impl wipple_frontend::Loader for Loader {
    fn std_path(&self) -> Option<wipple_frontend::FilePath> {
        None
    }

    fn resolve(
        &self,
        path: wipple_frontend::FilePath,
        _current: Option<wipple_frontend::FilePath>,
    ) -> anyhow::Result<wipple_frontend::FilePath> {
        if path.as_str() == "fuzz" {
            Ok(path)
        } else {
            Err(anyhow::Error::msg("no such file"))
        }
    }

    async fn load(&self, path: wipple_frontend::FilePath) -> anyhow::Result<Arc<str>> {
        if path.as_str() == "fuzz" {
            Ok(Arc::from(self.code.as_str()))
        } else {
            Err(anyhow::Error::msg("no such file"))
        }
    }

    fn virtual_paths(
        &self,
    ) -> wipple_util::Shared<HashMap<wipple_frontend::helpers::InternedString, Arc<str>>> {
        self.virtual_paths.clone()
    }

    fn cache(
        &self,
    ) -> wipple_util::Shared<
        HashMap<
            wipple_frontend::FilePath,
            Arc<wipple_frontend::analysis::ast::File<wipple_frontend::analysis::Analysis>>,
        >,
    > {
        self.cache.clone()
    }

    fn source_map(&self) -> wipple_util::Shared<wipple_frontend::SourceMap> {
        self.source_map.clone()
    }
}

fuzz_target!(|input: wipple_syntax::ast::File<Driver>| {
    fuzz(input);
});

#[tokio::main]
async fn fuzz(input: wipple_syntax::ast::File<Driver>) {
    let file = Driver::Fuzz(Arc::new(input))
        .syntax_of(None, None, ())
        .await
        .expect("failed to load file");

    let file = Arc::try_unwrap(file).unwrap();

    let fuzz_code = file.format().expect("failed to parse");

    let file = Driver::Code(fuzz_code.clone())
        .syntax_of(None, None, ())
        .await
        .expect("failed to load file");

    let file = Arc::try_unwrap(file).unwrap();

    let code = file.format().expect("failed to parse");

    let loader = Loader {
        code,
        ..Default::default()
    };

    let compiler = wipple_frontend::Compiler::new(loader);

    compiler
        .analyze_with(
            wipple_frontend::FilePath::Path(wipple_frontend::helpers::InternedString::new("fuzz")),
            &Default::default(),
        )
        .await;
}
