# goban-server-fastapi

## Initialize dev environment
```
docker-compose build
docker-compose run --rm django ./manage.py migrate
docker-compose run --rm django ./manage.py createsuperuser
```

## Run dev
```
docker-compose up
```