#!/bin/bash -e

if ! [ -z "$VITE_FIREBASE_AUTH_DOMAIN" ]; then
    echo "/__/auth/* https://$VITE_FIREBASE_AUTH_DOMAIN/__/auth/:splat 200" >> _site/_redirects
fi
