{-# LANGUAGE NamedFieldPuns #-}
module ReceivingHeaders (ReceivingHeaders(..), getReceivingHeaders) where

import qualified Data.Sequence as Seq

data ReceivingHeaders = ReceivingHeaders
    { playerNameI :: Int
    , recI :: Int
    , recYdsI :: Int
    , recTdsI :: Int
    , recFumblesI :: Int
    } deriving (Show)

getReceivingHeaders :: [[String]] -> Either String ReceivingHeaders
getReceivingHeaders headers = 
  case (playerNameIM, recIM, recYdsIM, recTdsIM, recFumblesIM) of
    (Just playerNameI, Just recI, Just recYdsI, Just recTdsI, Just recFumblesI) -> Right ReceivingHeaders
        { playerNameI, recI, recYdsI, recTdsI, recFumblesI}
    _ -> Left ("Could not get recing headers from " <> show header)
  where 
    header = Seq.fromList $ head headers
    playerNameIM = Seq.findIndexL ((==) "Player") header
    recIM = Seq.findIndexL ((==) "Rec") header
    recYdsIM = Seq.findIndexL ((==) "Yds") header
    recTdsIM = Seq.findIndexL ((==) "TD") header
    recFumblesIM = Seq.findIndexL ((==) "Rec FUM") header

