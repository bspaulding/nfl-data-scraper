{-# LANGUAGE DeriveGeneric #-}

module PlayersStats where

import Data.Aeson
import qualified Data.Map as Map
import GHC.Generics
import KickingStats
import PassingStats
import ReceivingStats
import RushingStats

type PlayersStats = Map.Map String PlayerStats

data PlayerStats = PlayerStats
  { passing :: PassingStats,
    rushing :: RushingStats,
    receiving :: ReceivingStats,
    kicking :: KickingStats
  }
  deriving (Generic, Show)

instance ToJSON PlayerStats

instance FromJSON PlayerStats
