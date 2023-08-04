#!/bin/bash

node scripts/dev-watcher 'task website' &
npx http-server -c-1 website/_site --cors &
wait
