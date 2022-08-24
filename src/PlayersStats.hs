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

defaultPlayerStats :: PlayerStats
defaultPlayerStats = PlayerStats
  { passing = defaultPassingStats
  , rushing = defaultRushingStats
  , receiving = defaultReceivingStats
  , kicking = defaultKickingStats
  }

passingCompletions :: PlayerStats -> Int
passingCompletions = completions . passing

passingYards :: PlayerStats -> Int
passingYards = PassingStats.passingYards . passing

passingTouchdowns :: PlayerStats -> Int
passingTouchdowns = PassingStats.passingTouchdowns . passing

passingInterceptions :: PlayerStats -> Int
passingInterceptions = PassingStats.interceptions . passing

rushingYards :: PlayerStats -> Int
rushingYards = RushingStats.rushingYards . rushing

rushingTouchdowns :: PlayerStats -> Int
rushingTouchdowns = RushingStats.rushingTouchdowns . rushing

fumbles :: PlayerStats -> Int
fumbles stats = (rushingFumbles . rushing) stats + (receivingFumbles . receiving) stats

receptions :: PlayerStats -> Int
receptions = ReceivingStats.receptions . receiving

receivingYards :: PlayerStats -> Int
receivingYards = ReceivingStats.receivingYards . receiving

receivingTouchdowns :: PlayerStats -> Int
receivingTouchdowns = ReceivingStats.receivingTouchdowns . receiving
