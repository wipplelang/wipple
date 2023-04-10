#!/bin/bash

set -e

if ! [[ -z $CI ]]; then
    brew install yq
fi

"$(dirname $(realpath $0))/wasm.sh"
npm install
npm run build

for element in ui/*/; do
    mkdir -p dist/$element
    (cd $element && npm i && npm run build && mv dist/* ../../dist/$element && rm -r dist)
done

for lesson in dist/lessons/learn/*.yml; do
    yq -p yaml -o json $lesson > ${lesson%.yml}.json
    rm $lesson
done
