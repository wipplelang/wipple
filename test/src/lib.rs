#![allow(missing_docs)]

use rstest::rstest;

#[rstest]
#[timeout(std::time::Duration::from_secs(10))]
#[tokio::test]
async fn tests(#[files("tests/**/*.test.wipple")] file: std::path::PathBuf) {
    use futures::{future, FutureExt};
    use serde::de::DeserializeOwned;
    use serde::Serialize;
    use std::{
        fs, io,
        path::Path,
        sync::{Arc, Mutex},
    };
    use wipple_driver::util::lazy_static::lazy_static;

    fn read_binary<T: DeserializeOwned>(path: impl AsRef<Path>) -> T {
        wipple_driver::util::read_binary(io::BufReader::new(fs::File::open(path).unwrap())).unwrap()
    }

    let test = async move {
        let file_name = file.file_name().unwrap().to_string_lossy().into_owned();

        let code = fs::read_to_string(&file).expect("failed to read file");

        let (should_compile, should_warn) = if code.starts_with("-- [should compile]") {
            (true, Some(false))
        } else if code.starts_with("-- [should warn]") {
            (true, Some(true))
        } else if code.starts_with("-- [should error]") {
            (false, None)
        } else {
            panic!(
                "expected test to begin with [should compile], [should warn], or [should error]"
            );
        };

        let file = wipple_driver::File {
            path: file.to_string_lossy().into_owned(),
            visible_path: wipple_driver::util::get_visible_path(&file),
            code,
        };

        let base_interface =
            read_binary::<wipple_driver::Interface>("../artifacts/base.wippleinterface");

        let base_library = read_binary::<wipple_driver::Library>("../artifacts/base.wipplelibrary");

        let result = wipple_driver::compile(vec![file], vec![base_interface.clone()]);

        let render = wipple_render::Render::new();
        render.update(
            vec![base_interface, result.interface],
            Some(result.library.clone()),
            None,
        );

        let compiled = Mutex::new(true);
        let compiled_with_warnings = Mutex::new(false);
        let mut rendered_diagnostics = result
            .diagnostics
            .into_iter()
            .map(|diagnostic| {
                let render = &render;
                let compiled = &compiled;
                let compiled_with_warnings = &compiled_with_warnings;

                let rendered_diagnostic = render
                    .render_diagnostic(&diagnostic)
                    .unwrap_or_else(|| panic!("could not render diagnostic: {diagnostic:#?}"));

                match rendered_diagnostic.severity {
                    wipple_render::RenderedDiagnosticSeverity::Error => {
                        *compiled.lock().unwrap() = false;
                    }
                    wipple_render::RenderedDiagnosticSeverity::Warning => {
                        *compiled_with_warnings.lock().unwrap() = true;
                    }
                }

                render.render_diagnostic_to_debug_string(&rendered_diagnostic)
            })
            .collect::<Vec<_>>();

        rendered_diagnostics.sort();
        rendered_diagnostics.dedup();

        let compiled = compiled.into_inner().unwrap();
        let compiled_with_warnings = compiled_with_warnings.into_inner().unwrap();

        assert_eq!(should_compile, compiled);

        if let Some(should_warn) = should_warn {
            assert_eq!(should_warn, compiled_with_warnings);
        }

        let output = if compiled {
            let executable =
                wipple_driver::link(vec![base_library, result.library]).expect("linking failed");

            #[derive(Clone)]
            struct Value;

            struct Runtime;

            lazy_static! {
                static ref RUNTIME: tokio::runtime::Runtime =
                    tokio::runtime::Builder::new_multi_thread()
                        .worker_threads(1)
                        .enable_all()
                        .build()
                        .unwrap();
            }

            impl wipple_interpreter::Runtime for Runtime {
                type Value = Value;
                type JoinHandle = tokio::task::JoinHandle<()>;

                fn run(
                    future: impl future::Future<Output = ()> + Send + 'static,
                ) -> Self::JoinHandle {
                    RUNTIME.spawn(future)
                }

                async fn from_value(
                    _value: wipple_interpreter::Value<Self>,
                    _task: &wipple_interpreter::TaskLocals<Self>,
                    _context: &wipple_interpreter::Context<Self>,
                ) -> Self::Value {
                    unimplemented!()
                }

                async fn to_value(_value: Self::Value) -> wipple_interpreter::Value<Self> {
                    unimplemented!()
                }

                async fn with_functions<T>(
                    _f: impl FnOnce(&mut Vec<wipple_interpreter::StoredFunction<Self>>) -> T + Send,
                ) -> T {
                    unimplemented!()
                }
            }

            let output: Arc<Mutex<Vec<String>>> = Default::default();

            let options = wipple_interpreter::Options::<Runtime>::with_io(wipple_interpreter::Io {
                display: Arc::new({
                    let output = output.clone();
                    move |message| {
                        let output = output.clone();
                        async move {
                            output.lock().unwrap().push(message);
                            Ok(())
                        }
                        .boxed()
                    }
                }),
                prompt: Arc::new(|_, _| unimplemented!()),
                choice: Arc::new(|_, _| unimplemented!()),
                ui: Arc::new(|_, _| unimplemented!()),
                sleep: Arc::new(|_| unimplemented!()),
            });

            if let Err(error) = wipple_interpreter::evaluate(executable, options).await {
                output.lock().unwrap().push(format!("error: {}\n", error.0));
            }

            let output = output.lock().unwrap();
            output.clone()
        } else {
            Vec::new()
        };

        #[derive(Default, Serialize)]
        struct Snapshot {
            diagnostics: Vec<String>,
            output: Vec<String>,
        }

        let snapshot = [rendered_diagnostics.join("\n"), output.join("\n")]
            .into_iter()
            .filter(|s| !s.is_empty())
            .collect::<Vec<_>>()
            .join("\n\n");

        insta::assert_snapshot!(file_name, snapshot);
    };

    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path("../snapshots");
    settings.bind_async(test).await;
}
