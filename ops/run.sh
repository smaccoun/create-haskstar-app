#!/usr/bin/env bash
cd ./back-end
../ttab ./run.sh 
../ttab ./docs/runDocServer.sh

cd ../front-end
../ttab yarn start

cd ..

