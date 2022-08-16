{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module SDIOPlayerStats where

import Data.Aeson
import GHC.Generics

data SDIOPlayerStats = SDIOPlayerStats
  { playerID :: Int,
    season :: Int,
    name :: String,
    position :: String,
    passingCompletions :: Float,
    passingYards :: Float,
    passingTouchdowns :: Float,
    passingInterceptions :: Float,
    rushingYards :: Float,
    rushingTouchdowns :: Float,
    fumbles :: Float,
    receptions :: Float,
    receivingYards :: Float,
    receivingTouchdowns :: Float
  }
  deriving (Generic, Show)

instance ToJSON SDIOPlayerStats

instance FromJSON SDIOPlayerStats where
  parseJSON = withObject "SDIOPlayerStats" $ \v ->
    SDIOPlayerStats
      <$> v .: "PlayerID"
      <*> v .: "Season"
      <*> v .: "Name"
      <*> v .: "Position"
      <*> v .: "PassingCompletions"
      <*> v .: "PassingYards"
      <*> v .: "PassingTouchdowns"
      <*> v .: "PassingInterceptions"
      <*> v .: "RushingYards"
      <*> v .: "RushingTouchdowns"
      <*> v .: "Fumbles"
      <*> v .: "Receptions"
      <*> v .: "ReceivingYards"
      <*> v .: "ReceivingTouchdowns"