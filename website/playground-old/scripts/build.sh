#!/bin/bash

set -e

"$(dirname $(realpath $0))/wasm.sh"
npm install
npm run build
npm run export
