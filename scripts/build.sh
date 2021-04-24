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

mkdir -p bin

cargo install cross

for target in ${TARGETS[@]}; do
    cross build --target=$target --release
    cp target/$target/release/wipple_bundled_interpreter bin/wipple-bundled-$target
    cp target/$target/release/wipple_cli bin/wipple-cli-$target
done
