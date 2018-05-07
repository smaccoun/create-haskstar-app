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

cd /tmp
rm -rf example-app
(env=DEVELOPMENT && hasm new example-app)
echo "FINISHED RUNNING"


cd ~/projects/haskstar/create-haskstar-app
cd ..
