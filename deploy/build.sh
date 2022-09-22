#!/bin/bash

sudo rm -rf dist
mkdir -p dist

./build-vue.sh
./build-django.sh
./build-devblog.sh
