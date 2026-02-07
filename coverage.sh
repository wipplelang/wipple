rm -rf coverage && mkdir coverage
eval "$(cargo llvm-cov show-env --sh 2>/dev/null)"
cargo llvm-cov clean --workspace
task test
cargo llvm-cov report --lcov --output-path=coverage/lcov.info
cargo llvm-cov report --json --output-path=coverage/coverage.json
cargo llvm-cov report --html --output-dir=coverage
cargo llvm-cov report --output-path=/dev/null --show-missing-lines | grep '/compiler/src/' > coverage/uncovered.txt
