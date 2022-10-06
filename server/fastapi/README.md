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

## Testing
The unit tests do not mock the database, so the postgresql database from the docker-compose environment must be up before tests can be run.

The database will be wiped before every test is run, so don't run tests in production.

```
poetry run pytest
```


## Code hygiene
```
poetry run black
poetry run isort
```
