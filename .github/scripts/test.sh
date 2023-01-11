#!/bin/bash

set -e

rustup update stable

mkdir junit-reports
cargo run --bin wipple-test tools/test/tests --junit > junit-reports/TEST-wipple.xml

for test in parsing compiling; do
    cargo run --bin wipple-fuzz -- --test $test --limit 5000 --quiet
done
