# nfl-data-scraper

## Commands

`nfl-data-scraper --command fetch --season <year>`

Fetches all passing, rushing, receiving, and kicking data for the specified year from NFL.com. Writes the output to a file named `player-data-<year>.json`.

`nfl-data-scraper --command export --season <year>`

Reformats `player-data-<year>.json`, writing to `nfl-data-export-<year>.json`.

