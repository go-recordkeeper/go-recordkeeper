FROM haskell:9.0 as build

WORKDIR /opt/haskell

# Install dependencies only for docker caching
COPY ./stack.yaml ./package.yaml /opt/haskell/
RUN stack update
RUN stack --version && ghc --version && cabal --version
RUN stack build --only-dependencies

COPY ./README.md ./CHANGELOG.md /opt/haskell/
COPY ./app/ /opt/haskell/app/
COPY ./src/ /opt/haskell/src/
COPY ./test/ /opt/haskell/test/
RUN stack install

# This is the ultimate base image for the haskell container, so that's what we'll use
FROM buildpack-deps:buster

COPY --from=build /root/.local/bin/go-recordkeeper-exe /root/.local/bin/go-recordkeeper-exe

CMD ["/root/.local/bin/go-recordkeeper-exe"]