FROM haskell:9.4.8-slim as builder
WORKDIR /builder
RUN cabal update
COPY cabal.project cabal.project
COPY nfl-data-scraper.cabal nfl-data-scraper.cabal
RUN cabal build --only-dependencies -j4
COPY app app
COPY src src

RUN cabal install

FROM debian:10-slim

RUN apt-get update && apt-get install -y ca-certificates

COPY --from=builder /root/.local/bin/nfl-data-scraper /usr/local/bin/nfl-data-scraper

WORKDIR /nfl-data-scraper

ENTRYPOINT ["nfl-data-scraper"]
