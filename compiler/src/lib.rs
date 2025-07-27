use crate::render::{RenderedDiagnostic, RenderedDocumentation};

pub mod codegen;
pub mod driver;
pub mod lower;
pub mod render;
pub mod syntax;
pub mod typecheck;
pub mod util;

pub type File = driver::File;

#[derive(Clone)]
pub struct Compiler {
    driver: crate::driver::Driver,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            driver: crate::driver::Driver::new(),
        }
    }

    pub fn format(code: &str) -> String {
        crate::driver::Driver::format(code)
    }

    pub fn compile(&mut self, files: Vec<File>) -> Vec<RenderedDiagnostic> {
        let diagnostics = self.driver.compile(files);

        let render = self.driver.render();

        diagnostics
            .into_iter()
            .filter_map(|diagnostic| render.render_diagnostic(&diagnostic))
            .collect()
    }

    pub fn documentation(&self, name: &str) -> Option<RenderedDocumentation> {
        let render = self.driver.render();
        let declaration = render.get_declaration_from_name(name)?;
        render.render_documentation(&declaration, false)
    }

    pub fn js_executable(&self, options: codegen::js::Options) -> Option<String> {
        self.driver
            .executable()
            .ok()
            .map(|executable| executable.to_js(options))
    }
}
