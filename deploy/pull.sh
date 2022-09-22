#!/bin/bash

if [[ ! -d goban-server-django ]]; then
  git clone https://github.com/go-recordkeeper/goban-server-django.git
fi

if [[ ! -d goban-vue ]]; then
  git clone https://github.com/go-recordkeeper/goban-vue.git
fi

if [[ ! -d devblog ]]; then
  git clone https://github.com/go-recordkeeper/devblog.git
fi

cd goban-server-django
git pull origin main
cd ..

cd goban-vue
git pull origin main
cd ..

cd devblog
git pull origin main
cd ..
