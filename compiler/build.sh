set -e

cargo test export_bindings_
rm -rf generated
for b in $(find . -name bindings); do
    mkdir -p generated/types/$(dirname $b)
    cp $b/*.ts generated/types/$(dirname $b)
    rm -r $b
done
for f in $(find generated/types -name '*.ts'); do
    namespace=$(basename $(dirname $f))

    if [[ $namespace != "util" ]]; then
        echo "export namespace $namespace {" >> generated/wipple-compiler.d.ts
    fi

    cat $f | grep -v '^import' >> generated/wipple-compiler.d.ts

    if [[ $namespace != "util" ]]; then
        echo "}" >> generated/wipple-compiler.d.ts
    fi
done
rm -r generated/types

rollup -c rollup.config.mjs
cp -r generated dist
cp *.d.ts dist
