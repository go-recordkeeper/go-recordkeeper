# goban-deploy

## Building

Building the web app takes a disastrously long time on a raspberry pi, so you should build static assets on a dev machine and commit dist to the repo:
```
./pull.sh
./build.sh
git add dist
git commit
```

## First deploy
```
./pull.sh
./generate-secret-key.sh
docker-compose build
docker-compose run django ./manage.py migrate
docker-compose run django ./manage.py createsuperuser
docker-compose up -d
```

## Redeploying
```
docker-compose stop
./pull.sh
# If any new pip packages are required
docker-compose build --no-cache
# If any database schema changes were made
docker-compose run django ./manage.py migrate
docker-compose up -d
```
