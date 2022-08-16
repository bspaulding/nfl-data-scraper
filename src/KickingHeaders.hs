{-# LANGUAGE NamedFieldPuns #-}

module KickingHeaders (KickingHeaders (..), getKickingHeaders) where

import qualified Data.Sequence as Seq

data KickingHeaders = KickingHeaders
  { playerNameI :: Int,
    fg0to19I :: Int,
    fg20to29I :: Int,
    fg30to39I :: Int,
    fg40to49I :: Int,
    fg50to59I :: Int,
    fg60PlusI :: Int
  }
  deriving (Show)

getKickingHeaders :: [[String]] -> Either String KickingHeaders
getKickingHeaders headers =
  case (playerNameIM, fg0to19IM, fg20to29IM, fg30to39IM, fg40to49IM, fg50to59IM, fg60PlusIM) of
    (Just playerNameI, Just fg0to19I, Just fg20to29I, Just fg30to39I, Just fg40to49I, Just fg50to59I, Just fg60PlusI) ->
      Right KickingHeaders {playerNameI, fg0to19I, fg20to29I, fg30to39I, fg40to49I, fg50to59I, fg60PlusI}
    _ -> Left ("Could not get rushing headers from " <> show header)
  where
    header = Seq.fromList $ head headers
    playerNameIM = Seq.findIndexL ("Player" ==) header
    fg0to19IM = Seq.findIndexL ("1-19 > A-M" ==) header
    fg20to29IM = Seq.findIndexL ("20-29 > A-M" ==) header
    fg30to39IM = Seq.findIndexL ("30-39 > A-M" ==) header
    fg40to49IM = Seq.findIndexL ("40-49 > A-M" ==) header
    fg50to59IM = Seq.findIndexL ("50-59 > A-M" ==) header
    fg60PlusIM = Seq.findIndexL ("60+ > A-M" ==) header