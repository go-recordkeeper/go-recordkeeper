#!/bin/bash

mkdir -p dist

docker run --rm -v $(pwd)/goban-vue:/goban-vue -w /goban-vue -v $(pwd)/dist:/dist node npm install && \
docker run --rm -v $(pwd)/goban-vue:/goban-vue -w /goban-vue -v $(pwd)/dist:/dist node npm run build-only -- --emptyOutDir --outDir /dist