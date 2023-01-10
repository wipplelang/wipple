#!/bin/bash

if ! command -v yq &> /dev/null; then
    echo "Error: yq not installed"
    exit 1
fi

if [[ -z $1 ]]; then
    echo -e "Error: please provide an output path"
    exit 1
fi

if [[ -d $1 ]]; then
    read -p "$1 already exists, press Enter to continue or Ctrl+C to exit..."
fi

rm -rf "$1"
mkdir "$1"
for f in tools/test/tests/*.yml; do
    f=$(basename $f)
    echo "Extracting $f"
    printf "%s\n" "$(cat "tools/test/tests/$f" | yq -r .code)" > "$1/${f%.yml}.wpl"
done

echo "Done"
