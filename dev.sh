#!/usr/bin/env bash

echo "Closing existing docker instances..."
docker stop my-app-db
docker rm my-app-db

cd ./core-script-src
stack build

cd ../

echo "Cleaning out existing dist directory and loading bundles"
rm -rf ./dist/*
cp logoAscii.txt core-script-src/.stack-work/install/x86_64-osx/lts-8.23/8.0.2/bin/create-app ./dist
cp -r templates ./dist

cd ./dist
(env=DEVELOPMENT && ./create-app example-app)
echo "FINISHED RUNNING"
rm -rf ../example-app
cp -r example-app ../
cd ..

