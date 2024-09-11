{-# LANGUAGE DeriveGeneric #-}

module PlayersStats where

import Data.Aeson
import qualified Data.Map as Map
import GHC.Generics
import KickingStats
import PassingStats
import ReceivingStats
import RushingStats
import PlayerInfo

type PlayersStats = Map.Map PlayerInfo PlayerStats

data PlayerStats = PlayerStats
  { passing :: PassingStats,
    rushing :: RushingStats,
    receiving :: ReceivingStats,
    kicking :: KickingStats
  }
  deriving (Generic, Show)

instance ToJSON PlayerStats where
  toEncoding = genericToEncoding defaultOptions

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

fieldGoalsMade0to19 :: PlayerStats -> Int
fieldGoalsMade0to19 = KickingStats.fieldGoalsMade0to19 . kicking
fieldGoalsMade20to29 :: PlayerStats -> Int
fieldGoalsMade20to29 = KickingStats.fieldGoalsMade20to29 . kicking
fieldGoalsMade30to39 :: PlayerStats -> Int
fieldGoalsMade30to39 = KickingStats.fieldGoalsMade30to39 . kicking
fieldGoalsMade40to49 :: PlayerStats -> Int
fieldGoalsMade40to49 = KickingStats.fieldGoalsMade40to49 . kicking
fieldGoalsMade50Plus :: PlayerStats -> Int
fieldGoalsMade50Plus stats =
  (KickingStats.fieldGoalsMade50to59 . kicking) stats
  + (KickingStats.fieldGoalsMade60Plus . kicking) stats

