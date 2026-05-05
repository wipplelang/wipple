#!/bin/bash -e

cargo build --lib --release --target wasm32-unknown-unknown
wasm-bindgen --target experimental-nodejs-module ./target/wasm32-unknown-unknown/release/wat_parser.wasm --out-dir ./pkg
