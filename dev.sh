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
rm -rf example-app
rm -rf ./dist/*

cd ./dist
(env=DEVELOPMENT && hasm new example-app)
echo "FINISHED RUNNING"
rm -rf ../example-app
cp -r example-app ../


mkdir /tmp/temp-hasm-elm
mv /tmp/hasm-elm-example/.git /tmp/temp-hasm-elm
rm -rf /tmp/hasml-elm-example
cp -r example-app /tmp/hasm-elm-example
cd /tmp/hasm-elm-example
mv /tmp/temp-hasm-elm/.git .
rm -rf /tmp/temp-hasm-elm
git aa
git cm "Check commit"
git po master
cd ~/projects/haskstar/create-haskstar-app
cd ..
