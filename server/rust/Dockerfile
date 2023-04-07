FROM rust:1.68 as build

WORKDIR /opt/rust

# Fetch dependencies first to speed up builds
# We inject a dummy main.rs because cargo commands won't run unless it's present
# However, we don't want the cache to invalidate whenever we change the real main.rs
COPY ./Cargo.toml ./Cargo.lock /opt/rust
COPY ./.cargo /opt/rust/.cargo
RUN --mount=type=cache,target=/root/.cargo \
  --mount=type=cache,target=/opt/rust/target \
  mkdir -p /opt/rust/src && \
  echo "fn main() {}" > /opt/rust/src/main.rs && \
  cargo fetch

COPY ./src /opt/rust/src
RUN --mount=type=cache,target=/root/.cargo \
  --mount=type=cache,target=/opt/rust/target \
  cargo install --frozen --offline --path .

# This is the ultimate base image for the haskell container, so that's what we'll use
FROM debian:buster-slim

COPY --from=build /usr/local/cargo/bin/go-recordkeeper /usr/local/bin/go-recordkeeper
CMD ["go-recordkeeper"]
