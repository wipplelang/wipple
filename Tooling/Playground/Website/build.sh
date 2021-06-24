#!/bin/bash

set -ex

# Build interpreter
../../../Library/WippleSources/_shim.sh
(cd .. && swift build -c release --static-swift-stdlib)
cp ../.build/release/WipplePlaygroundInterpreter ./functions/run/interpreter

# Build website
npm install && npm run build
(cd ./functions/run && npm install)
