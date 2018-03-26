#!/usr/bin/env bash
cd ./back-end
stack exec api-ex

cd ../front-end
yarn start &

cd ..

