#!/bin/bash -e

cargo build --lib --release --target wasm32-unknown-unknown -p wipple-api

WASM_PATH="../../target/wasm32-unknown-unknown/release/wipple_api.wasm"

echo "Running wasm-opt"
wasm-opt -Oz $WASM_PATH -o $WASM_PATH

for platform in "experimental-nodejs-module" "web"; do
    echo "Running wasm-bindgen for platform $platform"
    wasm-bindgen --target $platform --keep-debug $WASM_PATH --out-dir ./pkg/$platform
done

node build.js
