#!/bin/bash

set -e

tempdir="$(mktemp -d)"
echo -e "Working directory: $tempdir"

for f in ./tests/*.wpl; do
    echo -e "\033[1mTesting $f\033[0m"

    if (wipple compile -f go "$f" >/dev/null 2>"$tempdir/main.stderr"); then
        wipple compile -o "$tempdir/out" "$f" 2>"$tempdir/main.stderr"
        "$tempdir/out" || true
    fi
done
