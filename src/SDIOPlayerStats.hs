{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module SDIOPlayerStats where

import Data.Aeson
import GHC.Generics

data SDIOPlayerStats = SDIOPlayerStats
  { playerID :: Int,
    season :: Int,
    name :: String,
    position :: String,
    team :: String,
    passingCompletions :: Float,
    passingYards :: Float,
    passingTouchdowns :: Float,
    passingInterceptions :: Float,
    rushingYards :: Float,
    rushingTouchdowns :: Float,
    fumbles :: Float,
    receptions :: Float,
    receivingYards :: Float,
    receivingTouchdowns :: Float,
    twoPointConversionPasses :: Float,
    twoPointConversionRuns :: Float,
    twoPointConversionReceptions :: Float,
    extraPointsMade :: Float,
    extraPointsAttempted :: Float,
    played :: Int,
    fieldGoalsMade0to19 :: Float,
    fieldGoalsMade20to29 :: Float,
    fieldGoalsMade30to39 :: Float,
    fieldGoalsMade40to49 :: Float,
    fieldGoalsMade50Plus :: Float
  }
  deriving (Generic, Show)

instance ToJSON SDIOPlayerStats where
  toJSON (SDIOPlayerStats{..}) =
    object [
      "PlayerID" .= playerID,
      "Season" .= season,
      "Name" .= name,
      "Position" .= position,
      "Team" .= team,
      "PassingCompletions" .= passingCompletions,
      "PassingYards" .= passingYards,
      "PassingTouchdowns" .= passingTouchdowns,
      "PassingInterceptions" .= passingInterceptions,
      "RushingYards" .= rushingYards,
      "RushingTouchdowns" .= rushingTouchdowns,
      "Fumbles" .= fumbles,
      "Receptions" .= receptions,
      "ReceivingYards" .= receivingYards,
      "ReceivingTouchdowns" .= receivingTouchdowns,
      "TwoPointConversionPasses" .= twoPointConversionPasses,
      "TwoPointConversionRuns" .= twoPointConversionRuns,
      "TwoPointConversionReceptions" .= twoPointConversionReceptions,
      "ExtraPointsMade" .= extraPointsMade,
      "ExtraPointsAttempted" .= extraPointsAttempted,
      "Played" .= played,
      "FieldGoalsMade0to19" .= fieldGoalsMade0to19,
      "FieldGoalsMade20to29" .= fieldGoalsMade20to29,
      "FieldGoalsMade30to39" .= fieldGoalsMade30to39,
      "FieldGoalsMade40to49" .= fieldGoalsMade40to49,
      "FieldGoalsMade50Plus" .= fieldGoalsMade50Plus
     ]

instance FromJSON SDIOPlayerStats where
  parseJSON = withObject "SDIOPlayerStats" $ \v ->
    SDIOPlayerStats
      <$> v .: "PlayerID"
      <*> v .: "Season"
      <*> v .: "Name"
      <*> v .: "Position"
      <*> v .: "Team"
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
      <*> v .: "TwoPointConversionPasses"
      <*> v .: "TwoPointConversionRuns"
      <*> v .: "TwoPointConversionReceptions"
      <*> v .: "ExtraPointsMade"
      <*> v .: "ExtraPointsAttempted"
      <*> v .: "Played"
      <*> v .: "FieldGoalsMade0to19"
      <*> v .: "FieldGoalsMade20to29"
      <*> v .: "FieldGoalsMade30to39"
      <*> v .: "FieldGoalsMade40to49"
      <*> v .: "FieldGoalsMade50Plus"

