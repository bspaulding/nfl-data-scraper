{-# LANGUAGE DeriveGeneric #-}

module PlayerInfo where

import Data.Aeson
import GHC.Generics

data PlayerInfo = PlayerInfo { playerId :: String, team :: String, name :: String, position :: String } deriving (Generic, Eq, Ord, Show)

instance ToJSON PlayerInfo where
  toEncoding = genericToEncoding defaultOptions
instance ToJSONKey PlayerInfo

instance FromJSON PlayerInfo
instance FromJSONKey PlayerInfo


