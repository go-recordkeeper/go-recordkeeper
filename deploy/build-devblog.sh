#!/bin/bash

mkdir -p dist/blog

cp -r devblog/public/* dist/blog
# For reasons, the Hugo theme Gokarna's CSS style doesn't prepend /blog to the font URLs.
# To remediate this, just copy the fonts to the root.

mkdir -p dist/fonts
cp dist/blog/fonts/* dist/fonts
