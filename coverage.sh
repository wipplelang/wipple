eval "$(cargo llvm-cov show-env --sh 2>/dev/null)"
cargo llvm-cov clean --workspace
task test
cargo llvm-cov report --lcov --output-path lcov.info
