# Rust Implementation

## Test Coverage
```sh
RUSTFLAGS="-C instrument-coverage" cargo test
```

This will generate the `.profraw` profiling files. You will need to use something like [grcov](https://lib.rs/crates/grcov) to process that data into something more legible.
