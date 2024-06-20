#!/bin/bash -e

cd "$(dirname "$0")"

CHANNEL=$(cat ../../../rust-toolchain)
rustup toolchain install $CHANNEL
rustup component add rust-src --toolchain $CHANNEL
rustup run $CHANNEL wasm-pack build . --release --target web -- -Z build-std=panic_abort,std
