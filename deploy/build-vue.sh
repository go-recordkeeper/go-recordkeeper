#!/bin/bash

mkdir -p dist


docker run --rm -v $(pwd)/../client:/client -w /client/vue -v $(pwd)/dist:/outDir node npm install && \
docker run --rm -v $(pwd)/../client:/client -w /client/vue -v $(pwd)/dist:/outDir node npm run build-only -- --outDir /outDir --assetsDir static