#!/bin/bash -e

if ! [ -z "$WIPPLE_FIREBASE_AUTH_REDIRECT_URL" ]; then
    echo "/__/auth/* $WIPPLE_FIREBASE_AUTH_REDIRECT_URL/:splat 200" >> _site/_redirects
fi
