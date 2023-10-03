#!/bin/bash

task --watch website-playground-public-dev &
npx http-server -c-1 website/_site --cors &
wait
