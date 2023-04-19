# FastAPI Implementation

<p align="center">
<img src="https://go.chiquit.ooo/fastapi.png" width="400" />
</p>
<p align="center">
<img src="https://github.com/go-recordkeeper/go-recordkeeper/actions/workflows/fastapi.yml/badge.svg" />
</p>

Another Python implementation, this time using [FastAPI](https://fastapi.tiangolo.com/) and [SQLAlchemy](https://www.sqlalchemy.org/). This was the second implementation completed.

## Architecture
I had recently encountered [Vertical Slice Architecture](https://jimmybogard.com/vertical-slice-architecture/) and wanted to try it out with this implementation. The `goban_server_fastapi` module contains the `main.py` entrypoint and several common helper files like `db.py` and `settings.py` that are generally useful. The application is split up into two sections: `auth` for logging in and registering, and `records` for all the CRUD stuff. Within the `auth` and `records` modules are helpers that are only pertinent to that section. There are only three authentication endpoints (login, register, and get current user), so there is a single `auth/rest.py` that contains all three. There are substantially more record CRUD endpoints, so each one gets its own file in `records/rest/`. 

## Development

### Initialize dev environment
```
docker compose build
docker compose run --rm django ./manage.py migrate
docker compose run --rm django ./manage.py createsuperuser
```

### Run dev
```sh
docker compose up
```

### Testing
The unit tests do not mock the database, so the postgresql database from the docker compose environment must be up before tests can be run:
```sh
docker compose up -d postgres
```

The database will be wiped before every test is run, so don't run tests in production.

```sh
poetry run pytest
```


### Code hygiene
```sh
poetry run black goban_server_fastapi
poetry run isort goban_server_fastapi
```
