{-# LANGUAGE DeriveGeneric #-}

module RushingStats where

import Data.Aeson
import GHC.Generics

data RushingStats = RushingStats
  { rushingYards :: Int
  , rushingAttempts :: Int -- TODO: not used?
  , rushingTouchdowns :: Int
  , rushingFumbles :: Int
  } deriving (Generic, Show)

instance ToJSON RushingStats where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON RushingStats

defaultRushingStats :: RushingStats
defaultRushingStats = RushingStats 0 0 0 0
