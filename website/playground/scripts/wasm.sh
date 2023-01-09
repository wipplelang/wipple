#!/bin/bash

set -e

if ! [[ -z $CI ]]; then
    rustup toolchain install stable
    curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh -s -- -f
    FLAGS=""
else
    FLAGS="--features debug_playground"
fi

(cd runner && wasm-pack build -- $FLAGS)
