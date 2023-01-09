#!/bin/bash

set -e

if ! [[ -z $CI ]]; then
    rustup toolchain install stable
    curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
fi

if ! [[ -z $CI ]]; then
    FLAGS=""
else
    FLAGS="--features debug_playground"
fi

(cd runner && BINARYEN_PASS_DEBUG=1 wasm-pack build -- $FLAGS)
