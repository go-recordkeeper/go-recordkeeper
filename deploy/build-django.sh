#!/bin/bash

mkdir -p dist/static

docker compose run --volume $(pwd)/dist:/dist django ./manage.py collectstatic --no-input