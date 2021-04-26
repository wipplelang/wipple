#!/bin/bash

target=$1

mkdir -p bin

cargo install cross
cross build --target=$target --release

cp target/$target/release/wipple_bundled_interpreter bin/wipple-bundled-$target
cp target/$target/release/wipple_cli bin/wipple-cli-$target
