#!/bin/bash

set -e

(cd "$(dirname $(realpath $0))/../../../../tools/playground-runner" && wasm-pack build)
npm install
npm run build
