# goban-server-test
Integration testing for goban-server implementations

## Setup
```sh
git submodule init
git submodule update
poetry install
cp .env.template .env
```

See the "Testing local code" section for what to change in the `.env` file.

## Running tests
All tests
```sh
poetry run test
```

A specific test
```sh
poetry run test -k a_specific_test
```
A specific implementation
```sh
poetry run test -k "[django]"
```

## Submodules
git submodules are used to store the repositories that are tested. They need to be kept up to date, or else you will be running tests against outdated code. A simple `git pull` should pull any updates, which you can then commit. `git submodule update --remote` does the same thing, but is harder to remember. You can also navigate into the submodule and pull manually, since they behave exactly like normal git repositories.

### Testing local code
Sometimes you want to test code without committing and pushing it. You can do this by editing the `GOBAN_TEST_DIR_*` entries in your `.env` file. These variables are used by docker compose to determine which directory to pull the code being tested from. By putting a path to your local development repository, the test suite will use that code instead of the submodule code.

You may need to manually rebuild the container with, say, `docker compose build fastapi` to pick up dependency changes. The python implementations mount the code from the repository into the image so this shouldn't normally be necessary.

## Code hygiene
```sh
poetry run black goban_server_test
poetry run isort goban_server_test
```
