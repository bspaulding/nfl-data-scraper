module ScraperArgs (ScraperArgs(..), parseScraperArgs) where

import Options.Applicative

data ScraperArgs = ScraperArgs
  { cmd :: String
  , season :: Int
  , outputPath :: String
  , statsFilePath :: String
  }

scraperArgs :: Parser ScraperArgs
scraperArgs = ScraperArgs
          <$> strOption
              ( long "command"
              <> short 'c'
              <> help "Command to run, options are 'fetch' and 'export'")
          <*> option auto
              ( long "season"
                <> short 's'
                <> help "Season year to fetch or export" )
          <*> strOption
              ( long "output-path"
                <> short 'o'
                <> help "Path to write output to" )
          <*> strOption
              ( long "stats-file-path"
                <> short 'i'
                <> help "Path to read stats from. Ignored if command is 'fetch'." )

opts = info (scraperArgs <**> helper)
        (fullDesc
        <> progDesc "nfl-data-scraper -c [fetch|export] -s <year>"
        <> header "a nfl.com web scraper for getting player stats" )

parseScraperArgs = execParser opts
