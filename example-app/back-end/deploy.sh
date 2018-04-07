#!/bin/bash -e

SHA1=$1
DOCKER_BASE_DIR=smaccoun/haskstar-haskell
DOCKER_DIR="$DOCKER_BASE_DIR:$SHA1"
echo "$DOCKER_DIR"

docker build --rm=false -t "$DOCKER_DIR" -f Dockerfile .
echo "---- logging into ecr ----"
$(aws ecr get-login --region us-west-2)
echo "---- pushing to ecr ----"
docker push  "$DOCKER_DIR"
