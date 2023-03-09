FROM haskell:9.0 as build

WORKDIR /opt/haskell

# Install dependencies only for docker caching
RUN stack update
COPY ./stack.yaml ./package.yaml /opt/haskell/
RUN --mount=type=cache,target=/root/.stack \
  --mount=type=cache,target=/opt/haskell/.stack-work \
  stack build --only-dependencies --ghc-options="-j1"

COPY ./README.md ./CHANGELOG.md /opt/haskell/
COPY ./app/ /opt/haskell/app/
COPY ./src/ /opt/haskell/src/
COPY ./test/ /opt/haskell/test/
RUN --mount=type=cache,target=/root/.stack \
  --mount=type=cache,target=/opt/haskell/.stack-work \
  stack install --ghc-options="-j1"

# This is the ultimate base image for the haskell container, so that's what we'll use
FROM buildpack-deps:buster

COPY --from=build /root/.local/bin/go-recordkeeper-exe /root/.local/bin/go-recordkeeper-exe

CMD ["/root/.local/bin/go-recordkeeper-exe"]
