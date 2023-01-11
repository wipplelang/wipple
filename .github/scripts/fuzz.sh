#!/bin/bash

set -e

rustup update stable

for test in parsing compiling; do
    cargo run --bin wipple-fuzz -- --test $test --limit 5000
done
