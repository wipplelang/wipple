use internment::Intern;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct LoweredExternalReferenceExpr {
    pub namespace: Intern<String>,
    pub identifier: Intern<String>,
}

impl LoweredExternalReferenceExpr {
    pub fn new(namespace: Intern<String>, identifier: Intern<String>) -> Self {
        LoweredExternalReferenceExpr {
            namespace,
            identifier,
        }
    }
}
