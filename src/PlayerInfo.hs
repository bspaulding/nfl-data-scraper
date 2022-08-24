{-# LANGUAGE DeriveGeneric #-}

module PlayerInfo where

import Data.Aeson
import GHC.Generics

data PlayerInfo = PlayerInfo { name :: String, position :: String } deriving (Generic, Show)

instance ToJSON PlayerInfo

instance FromJSON PlayerInfo


