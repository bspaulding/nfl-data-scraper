{-# LANGUAGE NamedFieldPuns #-}
module RushingHeaders (RushingHeaders(..), getRushingHeaders) where

import qualified Data.Sequence as Seq

data RushingHeaders = RushingHeaders
    { playerNameI :: Int
    , rushYdsI :: Int
    , rushAttemptsI :: Int
    , rushTdsI :: Int
    , rushFumblesI :: Int
    } deriving (Show)

getRushingHeaders :: [[String]] -> Either String RushingHeaders
getRushingHeaders headers = 
  case (playerNameIM, rushYdsIM, rushAttemptsIM, rushTdsIM, rushFumblesIM) of
    (Just playerNameI, Just rushYdsI, Just rushAttemptsI, Just rushTdsI, Just rushFumblesI) -> Right RushingHeaders{ playerNameI, rushYdsI, rushAttemptsI, rushTdsI, rushFumblesI}
    _ -> Left ("Could not get rushing headers from " <> show header)
  where 
    header = Seq.fromList $ head headers
    playerNameIM = Seq.findIndexL ((==) "Player") header
    rushYdsIM = Seq.findIndexL ((==) "Rush Yds") header
    rushAttemptsIM = Seq.findIndexL ((==) "Att") header
    rushTdsIM = Seq.findIndexL ((==) "TD") header
    rushFumblesIM = Seq.findIndexL ((==) "Rush FUM") header
