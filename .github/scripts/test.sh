#!/bin/bash

set -e

rustup update stable

mkdir junit-reports
cargo run --bin wipple-test tools/test/tests --junit > junit-reports/TEST-wipple.xml
