#!/bin/bash -e

export CSC_IDENTITY_AUTO_DISCOVERY=false

rm -rf dist

if [[ -z "$CI" ]]; then
    npx electron-builder build
else
    npx electron-builder build --mac # TODO: More targets
    (cd dist/mas && zip -r ../mac.zip *)
    rm -r dist/mas
fi
