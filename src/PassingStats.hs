{-# LANGUAGE DeriveGeneric #-}

module PassingStats where

import Data.Aeson
import GHC.Generics

data PassingStats = PassingStats
  { passingYards :: Int,
    passAttempts :: Int,
    completions :: Int,
    passingTouchdowns :: Int,
    interceptions :: Int
  }
  deriving (Generic, Show)

instance ToJSON PassingStats

instance FromJSON PassingStats

defaultPassingStats :: PassingStats
defaultPassingStats = PassingStats 0 0 0 0 0
