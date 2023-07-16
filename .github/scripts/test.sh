#!/bin/bash

set -e

mkdir junit-reports
cargo run --bin wipple-test tests --junit > junit-reports/TEST-wipple.xml
