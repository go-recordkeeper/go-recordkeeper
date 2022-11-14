#!/bin/sh
cat $1 | docker compose exec -T postgres psql -U postgres default
