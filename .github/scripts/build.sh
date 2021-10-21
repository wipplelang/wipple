#!/bin/bash

set -e

target=$1
compiler=$2

mkdir -p bin

if [ "$compiler" = "cross" ]; then
    cargo install cross
fi

$compiler build --target=$target --release

cp target/$target/release/wipple bin/wipple-$target
