use serde::{de::DeserializeOwned, Serialize};
use std::io::{Read, Write};

/// Read a value from the binary format.
pub fn read_binary<T: DeserializeOwned>(r: impl Read) -> anyhow::Result<T> {
    let decompressed = zstd::decode_all(r)?;
    let value = serde_json::from_slice(&decompressed)?;
    Ok(value)
}

/// Write a value to the binary format.
pub fn write_binary(
    w: impl Write,
    value: &impl Serialize,
    compression_level: CompressionLevel,
) -> anyhow::Result<()> {
    let data = serde_json::to_vec(value)?;
    zstd::stream::copy_encode(data.as_slice(), w, compression_level.to_i32())?;
    Ok(())
}

/// The compression level of the binary.
pub enum CompressionLevel {
    /// Minimum compression.
    Min,

    /// Maximum compression.
    Max,
}

impl CompressionLevel {
    fn to_i32(&self) -> i32 {
        match self {
            CompressionLevel::Min => 1,
            CompressionLevel::Max => 22,
        }
    }
}
