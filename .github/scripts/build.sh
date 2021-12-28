#!/bin/bash

set -e

target=$1
compiler=$2

mkdir -p bin

rustup update stable

if [ "$compiler" = "cross" ]; then
    docker build -t wipple -f ./.github/dockerfiles/Dockerfile.$target ./.github/dockerfiles
    cargo install cross
fi

$compiler build --target=$target --release

cp target/$target/release/wipple bin/wipple-$target
cp target/$target/release/wipple-runner bin/wipple-runner-$target
