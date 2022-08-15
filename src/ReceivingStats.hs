{-# LANGUAGE DeriveGeneric #-}

module ReceivingStats where

import Data.Aeson
import GHC.Generics

data ReceivingStats = ReceivingStats 
    { receivingYards :: Int
    , receptions :: Int
    , receivingTouchdowns :: Int
    , receivingFumbles :: Int 
    } deriving (Generic, Show)

instance ToJSON ReceivingStats

defaultReceivingStats :: ReceivingStats
defaultReceivingStats = ReceivingStats 0 0 0 0

