# Rust Implementation

<p align="center" style="background: white;">
<img src="https://go.chiquit.ooo/rust.svg" width="200" />
</p>
<p align="center">
<img src="https://github.com/go-recordkeeper/go-recordkeeper/actions/workflows/rust.yml/badge.svg" />
</p>

A Rust implementation. I used [axum](https://docs.rs/axum/latest/axum/) for the REST API and [tokio-postgres](https://docs.rs/tokio-postgres/latest/tokio_postgres/) for the DB.

## Architecture
As a compiled language, this implementation has a lot in common with the [Haskell implementation](https://github.com/go-recordkeeper/go-recordkeeper/tree/main/server/haskell). The general layout is practically identical.

## Development

### Test Coverage
```sh
RUSTFLAGS="-C instrument-coverage" cargo test
```

This will generate the `.profraw` profiling files. You will need to use something like [grcov](https://lib.rs/crates/grcov) to process that data into something more legible.
