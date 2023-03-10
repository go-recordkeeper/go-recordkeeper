# goban-server-test
Integration testing for goban-server implementations

## Setup
```sh
git submodule init
git submodule update
poetry install
cp .env.template .env
```

## Running tests
All tests
```sh
poetry run test
```

A specific test
```sh
poetry run test -- -k a_specific_test
```
A specific implementation
```sh
poetry run test -- -k "[django]"
```

## Code hygiene
```sh
poetry run black goban_server_test
poetry run isort goban_server_test
```
