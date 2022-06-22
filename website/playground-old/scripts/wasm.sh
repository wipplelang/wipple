#!/bin/bash

set -e

if ! [[ -z $CI ]]; then
    rustup toolchain install stable
    curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
fi

(cd runner && wasm-pack build)
