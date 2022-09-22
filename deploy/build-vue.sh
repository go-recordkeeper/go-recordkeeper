#!/bin/bash

mkdir -p dist

docker run --rm -v $(pwd)/goban-vue:/goban-vue -w /goban-vue -v $(pwd)/dist:/outDir node npm install && \
docker run --rm -v $(pwd)/goban-vue:/goban-vue -w /goban-vue -v $(pwd)/dist:/outDir node npm run build-only -- --outDir /outDir --assetsDir static