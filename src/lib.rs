pub mod binary;
pub mod error;
pub mod text;

pub use binary::File as BinFile;
pub use error::{Error, Result};
pub use text::{File as TextFile, Source, Span};
