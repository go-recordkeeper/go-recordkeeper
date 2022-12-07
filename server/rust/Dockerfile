FROM rust:1.65 as build

WORKDIR /opt/rust

COPY . /opt/rust
RUN cargo install --path .

# This is the ultimate base image for the haskell container, so that's what we'll use
FROM debian:buster-slim

COPY --from=build /usr/local/cargo/bin/go-recordkeeper /usr/local/bin/go-recordkeeper
CMD ["go-recordkeeper"]
