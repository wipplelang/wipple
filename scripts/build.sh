#!/bin/bash

target=$1
cross=$2

mkdir -p bin

if [ "$cross" = "true" ]; then
    cargo install cross
    cross build --target=$target --release

    cp target/$target/release/wipple_bundled_interpreter bin/wipple-bundled-$target
    cp target/$target/release/wipple_cli bin/wipple-cli-$target
else
    cargo build --release

    cp target/release/wipple_bundled_interpreter bin/wipple-bundled-$target
    cp target/release/wipple_cli bin/wipple-cli-$target
fi
