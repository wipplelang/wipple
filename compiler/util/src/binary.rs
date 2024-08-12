use serde::{de::DeserializeOwned, Serialize};
use std::io::{Read, Write};

const COMPRESSION_LEVEL: i32 = 22; // maximum compression level

/// Read a value from the binary format.
pub fn read_binary<T: DeserializeOwned>(r: impl Read) -> anyhow::Result<T> {
    let decompressed = zstd::decode_all(r)?;
    let value = serde_json::from_slice(&decompressed)?;
    Ok(value)
}

/// Write a value to the binary format.
pub fn write_binary(w: impl Write, value: &impl Serialize) -> anyhow::Result<()> {
    let data = serde_json::to_vec(value)?;
    zstd::stream::copy_encode(data.as_slice(), w, COMPRESSION_LEVEL)?;
    Ok(())
}
