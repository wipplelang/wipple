#!/bin/bash

case $1 in
    macos)
        TARGETS=(
            "x86_64-apple-darwin"
        )
        ;;
    linux)
        TARGETS=(
            "i686-unknown-linux-gnu"
            "x86_64-unknown-linux-gnu"
            "armv7-unknown-linux-gnueabihf"
            "aarch64-unknown-linux-gnu"
        )
        ;;
esac

git clone https://github.com/wipplelang/cli.git
cd cli

mkdir -p bin

cargo install cross

for target in ${TARGETS[@]}; do
    rustup target add $target
    cross build --target=$target --release
    cp target/$target/release/cli bin/$target
done
