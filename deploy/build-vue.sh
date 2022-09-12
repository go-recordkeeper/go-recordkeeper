#!/bin/bash

mkdir -p dist/static

docker run --rm -v $(pwd)/goban-vue:/goban-vue -w /goban-vue -v $(pwd)/dist/static:/outDir node npm install && \
docker run --rm -v $(pwd)/goban-vue:/goban-vue -w /goban-vue -v $(pwd)/dist/static:/outDir node npm run build-only -- --emptyOutDir --outDir /outDir --base /static/