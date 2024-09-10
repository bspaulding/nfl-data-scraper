FROM haskell:9.4.8-slim as builder
WORKDIR /builder
RUN cabal update
COPY cabal.project cabal.project
COPY nfl-data-scraper.cabal nfl-data-scraper.cabal
COPY app app
COPY src src

RUN --mount=type=cache,target=$HOME/.ghcup \
    --mount=type=cache,target=$HOME/.cabal \
    --mount=type=cache,target=$HOME/.ghc \
    --mount=type=cache,target=/builder/dist-newstyle \
    cabal build
RUN cabal install

FROM debian:10-slim

COPY --from=builder /root/.local/bin/nfl-data-scraper /usr/local/bin/nfl-data-scraper
RUN apt-get update && apt-get install -y ca-certificates

ENTRYPOINT ["nfl-data-scraper"]
