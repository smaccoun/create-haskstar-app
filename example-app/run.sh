#!/usr/bin/env bash
cd ./back-end
./ttab && stack exec api-ex

cd ../front-end
./ttab yarn start

cd ..

