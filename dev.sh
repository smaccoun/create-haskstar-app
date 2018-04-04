#!/usr/bin/env bash

if [ -z "$1" ]
  then
     APP_NAME="example-app"
  else
    APP_NAME="$1"
fi

echo "CREATING $APP_NAME"

echo "Closing existing docker instances..."
docker stop my-app-db
docker rm my-app-db

cd ./core-script-src
stack install

cd ../

echo "Cleaning out existing dist directory and loading bundles"
rm -rf ./dist/*

cd ./dist
(env=DEVELOPMENT && ~/.local/bin/create-app example-app)
echo "FINISHED RUNNING"
rm -rf ../example-app
cp -r example-app ../
cd ..
