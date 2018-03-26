#!/usr/bin/env bash

COMMIT_MESSAGE=$1
echo "Deploying with commit message $COMMIT_MESSAGE"

./build.sh

git add --all

git commit -m "$COMMIT_MESSAGE"

git push origin master




