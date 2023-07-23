#!/bin/bash

set -e

(cd "../../tools/playground-runner" && wasm-pack build)
npm install
npm run build

(cd dist && zip -r dist.zip * **/*)
