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

    pub fn compile(&mut self, files: Vec<File>) -> Vec<serde_json::Value> {
        let diagnostics = self.driver.compile(files);

        let render = self.driver.render();

        diagnostics
            .into_iter()
            .filter_map(|diagnostic| {
                Some(serde_json::to_value(&render.render_diagnostic(&diagnostic)?).unwrap())
            })
            .collect()
    }

    pub fn documentation(&self, name: &str) -> Option<serde_json::Value> {
        let render = self.driver.render();
        let declaration = render.get_declaration_from_name(name)?;
        let documentation = render.render_documentation(&declaration, false)?;
        serde_json::to_value(documentation).ok()
    }

    pub fn js_executable(&self) -> Option<String> {
        self.driver
            .executable()
            .ok()
            .map(|executable| executable.to_js())
    }
}
