{-# LANGUAGE RecordWildCards #-}

module NFLUrlParams where

import NFLDataCategory

data NFLUrlParams = NFLUrlParams { year :: Int, category :: NFLDataCategory } deriving (Show)

host :: String
host = "https://www.nfl.com"

nflUrl :: NFLUrlParams -> String
nflUrl NFLUrlParams{..} = host <> "/stats/player-stats/category/" <> toParam category <> "/" <> show year <> "/pre/all/" <> sortOrder category <> "/DESC"

