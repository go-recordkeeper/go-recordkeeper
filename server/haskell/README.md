# Haskell Implementation

<p align="center">
<img src="https://go.chiquit.ooo/haskell.svg" width="400" />
![Haskell](https://github.com/go-recordkeeper/go-recordkeeper/actions/workflows/haskell.yml/badge.svg)
</p>

I'm using `stack` for package management, so you will need `ghc`, `cabal` and `stack` installed. I recommend [GHCup](https://www.haskell.org/ghcup/) for installing Haskell things.

## Initialize dev environment
```sh
docker compose build
docker compose run --rm django ./manage.py migrate
docker compose run --rm django ./manage.py createsuperuser
```

## Building
```sh
stack build
```
