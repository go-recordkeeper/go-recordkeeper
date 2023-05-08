#!/bin/bash

if [ ! -f .env ]; then
  cp .env.template .env
fi

length=100

echo -n 'GOBAN_SECRET_KEY=' >> .env
tr -dc A-Za-z0-9 </dev/urandom | head -c $length >> .env
echo '' >> .env
