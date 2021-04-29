#!/bin/bash

target=$1
compiler=$2

mkdir -p bin

if [ "$compiler" = "cross" ]; then
    cargo install cross
fi

$compiler build --target=$target --release

cp target/$target/release/wipple_bundled_interpreter bin/wipple-bundled-$target
cp target/$target/release/wipple_cli bin/wipple-cli-$target
