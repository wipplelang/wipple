#!/bin/bash

set -e

mkdir junit-reports
cargo run --bin wipple-test -- --junit tests/*.wpl > junit-reports/TEST-wipple.xml
