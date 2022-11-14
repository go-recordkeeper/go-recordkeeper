#!/bin/sh
docker compose exec postgres pg_dump -c -U postgres default --no-owner > $1
