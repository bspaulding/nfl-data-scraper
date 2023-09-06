{-# LANGUAGE NamedFieldPuns #-}
module PlayerInfoHeaders (PlayerInfoHeaders(..), getPlayerInfoHeaders) where

import qualified Data.Sequence as Seq

data PlayerInfoHeaders = PlayerInfoHeaders
    { playerIdI :: Int
    , teamI :: Int
    , positionI :: Int
    , playerNameI :: Int
    } deriving (Eq, Show)

getPlayerInfoHeaders :: [[String]] -> Either String PlayerInfoHeaders
getPlayerInfoHeaders headers  =
  case (playerIdM, teamIM, positionIM, playerNameIM) of
    (Just playerIdI, Just teamI, Just positionI, Just playerNameI) -> Right PlayerInfoHeaders{ playerIdI, teamI, positionI, playerNameI }
    _ -> Left ("Could not get player info headers from " <> show header)
  where
    header = Seq.fromList $ head headers
    playerIdM = Seq.findIndexL ((==) "playerId") header
    teamIM = Seq.findIndexL ((==) "team") header
    positionIM = Seq.findIndexL ((==) "position") header
    playerNameIM = Seq.findIndexL ((==) "Player") header

