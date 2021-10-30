use serde::Serialize;
use wipple_diagnostics::Span;
use wipple_parser::LocalIntern;

#[derive(Debug, Clone, Copy, Serialize)]
pub struct DebugInfo {
    pub span: Span,
    pub declared_name: Option<LocalIntern<String>>,
}
