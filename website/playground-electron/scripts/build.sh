#!/bin/bash -e

export CSC_IDENTITY_AUTO_DISCOVERY=false

if [[ -z "$CI" ]]; then
    npx electron-builder build
else
    npx electron-builder build --mac # TODO: More targets
fi
