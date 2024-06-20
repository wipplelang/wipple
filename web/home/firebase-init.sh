#!/bin/bash -e

if ! [ -z "$VITE_FIREBASE_AUTH_INIT_JSON" ]; then
    mkdir -p _site/__/firebase
    echo "$VITE_FIREBASE_AUTH_INIT_JSON" > _site/__/firebase/init.json
fi
