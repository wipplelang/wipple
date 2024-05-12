#!/bin/bash

set -e

echo -n "Exporting TypeScript bindings..."
cargo test export_bindings_ --quiet > /dev/null
rm -rf generated
for b in $(find . -name bindings); do
    mkdir -p generated/types/$(dirname $b)
    cp $b/*.ts generated/types/$(dirname $b)
    rm -r $b
done
for f in $(find generated/types -name '*.ts'); do
    cat $f | grep -v '^import' >> generated/wipple-compiler.d.ts
done
rm -r generated/types
echo " done"

echo -n "Building compiler..."
rollup -c rollup.config.mjs
cp -r generated dist
cp *.d.ts dist
echo " done"
