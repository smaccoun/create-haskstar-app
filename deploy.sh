#!/usr/bin/env bash

COMMIT_MESSAGE=$1
echo COMMIT_MESSAGE

./build.sh

git add --all

git commit -m COMMIT_MESSAGE

git push origin master




