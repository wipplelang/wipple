#!/bin/bash -e

cargo update -p wasm-bindgen
cargo build --lib --release --target wasm32-unknown-unknown -p wipple-api

WASM_PATH="../../target/wasm32-unknown-unknown/release/wipple_api.wasm"

echo "Running wasm-opt"
wasm-opt -Oz $WASM_PATH -o $WASM_PATH

echo "Running wasm-bindgen"
wasm-bindgen --target web $WASM_PATH --out-dir ./pkg
