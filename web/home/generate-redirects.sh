#!/bin/bash -e

if ! [ -z "$VITE_FIREBASE_PROJECT_ID" ]; then
    echo "/__/auth/* https://$VITE_FIREBASE_PROJECT_ID.firebaseapp.com/__/auth/:splat 200" >> _site/_redirects
fi
