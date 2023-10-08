#!/bin/bash

set -e

tempdir="$(mktemp -d)"
echo -e "Working directory: $tempdir"

for f in ./tests/*.wpl; do
    echo -e "Testing $f"

    if (cargo run --bin wipple -- compile -f go "$f" > "$tempdir/main.go"); then
        cargo run --bin wipple -- compile -o "$tempdir/out" "$f"
    fi

    "$tempdir/out"
done
