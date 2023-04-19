#!/bin/bash

set -e

"$(dirname $(realpath $0))/wasm.sh"
npm install
npm run build

for lesson in dist/lessons/*.txt dist/lessons/**/*.txt; do
    cat $lesson | node $(dirname $(realpath $0))/convert-lesson.js > ${lesson%.txt}.json
    rm $lesson
done

for element in ui/*/; do
    mkdir -p dist/$element
    (cd $element && npm i && npm run build && mv dist/* ../../dist/$element && rm -r dist)
done
