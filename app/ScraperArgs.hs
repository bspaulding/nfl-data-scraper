module ScraperArgs (ScraperArgs(..), parseScraperArgs) where

import Options.Applicative --(fullDesc, Parser, header, progDesc, helper, help, info, long, (<**>), short, auto, option, strOption)

data ScraperArgs = ScraperArgs
  { cmd :: String
  , season :: Int
  }

scraperArgs :: Parser ScraperArgs
scraperArgs = ScraperArgs
          <$> strOption
              ( long "command"
              <> help "Command to run, options are 'fetch' and 'export'")
          <*> option auto
              ( long "season"
                <> short 's'
                <> help "Season year to fetch or export" )

opts = info (scraperArgs <**> helper)
        (fullDesc
        <> progDesc "nfl-data-scraper [fetch|export] <year>"
        <> header "a nfl.com web scraper for getting player stats" )

parseScraperArgs = execParser opts
