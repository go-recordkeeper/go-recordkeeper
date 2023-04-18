# Django implementation

<p align="center">
<img src="https://go.chiquit.ooo/django.png" width="300" />
</p>
<p align="center">
<img src="https://github.com/go-recordkeeper/go-recordkeeper/actions/workflows/django.yml/badge.svg" />
</p>

Written entirely in Django. This was the first server written and serves as the reference implementation.

## Migrations
In addition to acting as the de facto reference, the database schema is also managed using Django's migration system. Any changes to the schema should be made here first, added as a migration, and propagated to other implementations after testing.

## Development

### Virtual environment
You should create a fresh virtual environment using whatever tool you prefer.

```sh
# Install the application dependencies
pip install -e .
# Install optional testing dependencies as well
pip install -e ".[test]"
```

### Development instance
The development server and database are both managed by docker compose.

```sh 
docker compose build # build the django container
docker compose up -d # bring up the dev server and the postgres database in the background
docker compose stop # stop the dev server and the postgres database
docker compose run --rm django python manage.py migrate # set up the database tables
docker compose run --rm django python manage.py createsuperuser # set up an admin user
```

### Testing and code hygiene
I use `tox` to manage all the formatting and testing tools. Simply `pip install tox` to get it.

```sh
tox # lint and run tests
tox -e lint # only lint
tox -e format # format all code using black and isort
tox -e test # run tests
tox -e test -- -k name_of_test # run a specific test
```

The tests require a postgres instance to be running. You can run `docker compose up -d postgres` to bring up the development database in the background.
