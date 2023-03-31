#!/bin/bash

set -e

"$(dirname $(realpath $0))/wasm.sh"
npm install
npm run build

for element in ui/*/; do
    mkdir -p dist/$element
    (cd $element && npm run build && mv dist/* ../../dist/$element && rm -r dist)
done
