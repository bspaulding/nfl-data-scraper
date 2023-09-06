{-# LANGUAGE NamedFieldPuns #-}
module PassingHeaders (PassingHeaders(..), getPassingHeaders) where

import qualified Data.Sequence as Seq

data PassingHeaders = PassingHeaders
    { passYdsI :: Int
    , passAttemptsI :: Int
    , passTdsI :: Int
    , intsI :: Int
    , completionsI :: Int
    } deriving (Show)

getPassingHeaders :: [[String]] -> Either String PassingHeaders
getPassingHeaders headers  =
  case (passYdsIM, passAttemptsIM, passTdsIM, intsIM, completionsIM) of
    (Just passYdsI, Just passAttemptsI, Just passTdsI, Just intsI, Just completionsI) -> Right PassingHeaders{ passYdsI, passAttemptsI, passTdsI, intsI, completionsI }
    _ -> Left ("Could not get passing headers from " <> show header)
  where
    header = Seq.fromList $ head headers
    passYdsIM = Seq.findIndexL ((==) "Pass Yds") header
    passAttemptsIM = Seq.findIndexL ((==) "Att") header
    passTdsIM = Seq.findIndexL ((==) "TD") header
    intsIM = Seq.findIndexL ((==) "INT") header
    completionsIM = Seq.findIndexL ((==) "Cmp") header
