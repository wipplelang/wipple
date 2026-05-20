#!/bin/bash -e

cargo build --lib --release --target wasm32-unknown-unknown -p wipple-api
wasm-bindgen --target experimental-nodejs-module --keep-debug ../../target/wasm32-unknown-unknown/release/wipple_api.wasm --out-dir ./pkg
node build.js
