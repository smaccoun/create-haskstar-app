#!/usr/bin/env bash

(env $(cat .env | xargs) stack exec -- pg-simple-exe )
