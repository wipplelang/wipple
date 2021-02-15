#[derive(Debug, Clone)]
pub struct ProgramError {
    pub message: String,
}

impl ProgramError {
    pub fn new(message: &str) -> ProgramError {
        ProgramError {
            message: String::from(message),
        }
    }
}
