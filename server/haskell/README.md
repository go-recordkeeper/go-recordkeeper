# Haskell Implementation

<p align="center">
<img src="https://go.chiquit.ooo/haskell.svg" width="400" />
</p>
<p align="center">
<img src="https://github.com/go-recordkeeper/go-recordkeeper/actions/workflows/haskell.yml/badge.svg" />
</p>

A Haskell implementation. I used [scotty](https://hackage.haskell.org/package/scotty) for the REST API and [hasql](https://hackage.haskell.org/package/hasql) for the DB. This was the third implementation completed. 

## Architecture
The file layout is quite reminiscent of the [FastAPI](https://github.com/go-recordkeeper/go-recordkeeper/tree/main/server/fastapi) implementation; Vertical slices, `Auth` and `Record` sections, the same set of helpers, and a file for every endpoint.

This was the first implementation I did not use an ORM, so there is a significant chunk of SQL in every endpoint.

## Development

I'm using `stack` for package management, so you will need `ghc`, `cabal` and `stack` installed. I recommend [GHCup](https://www.haskell.org/ghcup/) for installing Haskell things.

### Initialize dev environment
```sh
docker compose build
docker compose run --rm django ./manage.py migrate
docker compose run --rm django ./manage.py createsuperuser
```

### Building
```sh
stack build
```
