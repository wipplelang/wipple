#!/bin/bash

set -e

mkdir junit-reports
cargo run --bin wipple-test tools/test/tests --junit > junit-reports/TEST-wipple.xml
