#!/bin/bash

set -e

"$(dirname $(realpath $0))/wasm.sh"
npm install
npm run build

mkdir -p dist/doc

for element in ui/*/; do
    mkdir -p dist/$element
    (cd $element && npm i && npm run build && mv dist/* ../../dist/$element && rm -r dist)
done

mkdir -p dist/publish
(cd publish && npm i && npm run build && mv dist/* ../dist/publish && rm -r dist)
