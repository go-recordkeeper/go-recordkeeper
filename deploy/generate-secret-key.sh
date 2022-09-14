#!/bin/bash

envfile=".env.django"
length=100

echo -n 'GOBAN_SECRET_KEY=' > $envfile
tr -dc A-Za-z0-9 </dev/urandom | head -c $length >> $envfile
echo '' >> $envfile