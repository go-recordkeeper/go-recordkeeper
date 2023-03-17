# FastAPI Implementation

## Initialize dev environment
```
docker-compose build
docker-compose run --rm django ./manage.py migrate
docker-compose run --rm django ./manage.py createsuperuser
```

## Run dev
```sh
docker-compose up
```

## Testing
The unit tests do not mock the database, so the postgresql database from the docker-compose environment must be up before tests can be run:
```sh
docker-compose up -d postgres
```

The database will be wiped before every test is run, so don't run tests in production.

```sh
poetry run pytest
```


## Code hygiene
```sh
poetry run black goban_server_fastapi
poetry run isort goban_server_fastapi
```
