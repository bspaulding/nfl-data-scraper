{-# LANGUAGE DeriveGeneric #-}

module KickingStats where

import Data.Aeson
import GHC.Generics

data KickingStats = KickingStats
  { fieldGoalsMade0to19 :: Int,
    fieldGoalsMade20to29 :: Int,
    fieldGoalsMade30to39 :: Int,
    fieldGoalsMade40to49 :: Int,
    fieldGoalsMade50to59 :: Int,
    fieldGoalsMade60Plus :: Int
  }
  deriving (Generic, Show)

instance ToJSON KickingStats

instance FromJSON KickingStats

defaultKickingStats :: KickingStats
defaultKickingStats = KickingStats 0 0 0 0 0 0
