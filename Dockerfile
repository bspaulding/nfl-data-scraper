FROM haskell:9.4.8-slim as builder
RUN cabal update
COPY cabal.project cabal.project
COPY nfl-data-scraper.cabal nfl-data-scraper.cabal
COPY app app
COPY src src

RUN cabal build
RUN cabal install

FROM debian:10-slim

COPY --from=builder /root/.local/bin/nfl-data-scraper /usr/local/bin/nfl-data-scraper

ENTRYPOINT ["nfl-data-scraper"]
