#!/bin/bash

if [[ ! -d goban-python ]]; then
  git clone https://github.com/dchiquito/goban-python.git
fi

if [[ ! -d goban-vue ]]; then
  git clone https://github.com/dchiquito/goban-vue.git
fi

cd goban-python
git pull origin main
cd ..

cd goban-vue
git pull origin main
cd ..

