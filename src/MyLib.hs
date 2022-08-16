{-# LANGUAGE RecordWildCards #-}

module MyLib (fetchPassingData, fetchRushingData, fetchReceivingData, fetchKickingData, fetchPlayerData, NFLDataCategory (..)) where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import KickingHeaders
import KickingStats
import NFLDataCategory
import NFLUrlParams
import Network.HTTP.Simple
import PassingHeaders (PassingHeaders (..), getPassingHeaders)
import PassingStats
import PlayersStats
import ReceivingHeaders (ReceivingHeaders (..), getReceivingHeaders)
import ReceivingStats
import RushingHeaders (RushingHeaders (..), getRushingHeaders)
import RushingStats
import Text.XML.HXT.Core

filterEmptyRows :: Int -> [String] -> [String]
filterEmptyRows n xs =
  concat $ takeWhile goodRow (chunksOf n xs)
  where
    goodRow :: [String] -> Bool
    goodRow = not . Char.isDigit . head . head

-- (header cells, row cells, next page link)
fetchPage :: String -> IO ([String], [String], [String])
fetchPage url = do
  putStrLn $ "Fetching url = " <> url
  request <- parseRequest ("GET " <> url)
  response <- httpLBS request
  let body = getResponseBody response
  let bodyStr = L8.unpack body
  let doc = readString [withParseHTML yes, withWarnings no] bodyStr
  headers <- runX $ doc //> hasName "thead" //> hasName "th" //> getText
  rows <- runX $ doc //> hasName "tbody" //> hasName "td" //> getText
  putStrLn $ "length of headers = " <> show (length headers) <> ", length of rows = " <> show (length rows)
  nextPageLink <- runX $ doc //> hasAttrValue "class" (== "nfl-o-table-pagination__next") >>> getAttrValue "href"
  if ((length rows) `mod` (length headers)) /= 0
    then return (headers, filterEmptyRows (length headers) rows, [])
    else return (headers, rows, nextPageLink)

-- First are the table headers, then the concat'd rows from each table
type RawData = ([[String]], [String])

fetchAllPages :: String -> IO RawData
fetchAllPages url = fetchAllPagesR url ([], [])

fetchAllPagesR :: String -> RawData -> IO RawData
fetchAllPagesR url (headers, rows) = do
  (headers', rows', next) <- fetchPage url
  let data' = (headers ++ [headers'], rows ++ rows')
  if length next > 0
    then fetchAllPagesR (host <> head next) data'
    else return data'

checkHeaders :: RawData -> Either String RawData
checkHeaders (headers, rows) =
  if any (/= (head headers)) (tail headers)
    then Left ("Headers were not consistent! " <> show headers)
    else Right (headers, rows)

transformPassingData :: RawData -> Either String (Map.Map String PassingStats)
transformPassingData (headers', rows) =
  case getPassingHeaders headers' of
    Left e -> Left e
    Right headers -> Right $ foldl (insertStats headers) Map.empty (chunkedRows (headers', rows))
  where
    insertStats :: PassingHeaders -> (Map.Map String PassingStats) -> [String] -> (Map.Map String PassingStats)
    insertStats headers acc row =
      Map.insert (strip (row !! (PassingHeaders.playerNameI headers))) (stats headers row) acc

    stats :: PassingHeaders -> [String] -> PassingStats
    stats hs row =
      PassingStats
        { passingYards = read $ row !! (passYdsI hs),
          passAttempts = read $ row !! (passAttemptsI hs),
          passingTouchdowns = read $ row !! (passTdsI hs),
          interceptions = read $ row !! (intsI hs),
          completions = read $ row !! (completionsI hs)
        }

transformRushingData :: RawData -> Either String (Map.Map String RushingStats)
transformRushingData (headers', rows) =
  case getRushingHeaders headers' of
    Left e -> Left e
    Right headers -> Right $ foldl (insertStats headers) Map.empty (chunkedRows (headers', rows))
  where
    insertStats :: RushingHeaders -> (Map.Map String RushingStats) -> [String] -> (Map.Map String RushingStats)
    insertStats headers acc row =
      Map.insert (strip (row !! (RushingHeaders.playerNameI headers))) (stats headers row) acc

    stats :: RushingHeaders -> [String] -> RushingStats
    stats hs row =
      RushingStats
        { rushingYards = read $ row !! (rushYdsI hs),
          rushingAttempts = read $ row !! (rushAttemptsI hs),
          rushingTouchdowns = read $ row !! (rushTdsI hs),
          rushingFumbles = read $ row !! (rushFumblesI hs)
        }

chunkedRows :: RawData -> [[String]]
chunkedRows (headers', rows) = (chunksOf ((length . head) headers') rows)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = (take n xs) : (chunksOf n (drop n xs))

strip :: String -> String
strip = T.unpack . T.strip . T.pack

fetchData :: NFLDataCategory -> Int -> IO (Either String RawData)
fetchData category year = do
  let url = nflUrl (NFLUrlParams {year = year, category = category})
  result <- fetchAllPages url
  print result
  return $ checkHeaders result

type PlayersPassingStats = Map.Map String PassingStats

fetchPassingData :: Int -> IO (Either String PlayersPassingStats)
fetchPassingData year = do
  rawData <- fetchData Passing year
  case rawData of
    Left e -> return $ Left e
    Right rawData' -> return $ transformPassingData rawData'

type PlayersRushingStats = Map.Map String RushingStats

fetchRushingData :: Int -> IO (Either String PlayersRushingStats)
fetchRushingData year = do
  rawData <- fetchData Rushing year
  case rawData of
    Left e -> return $ Left e
    Right rawData' -> return $ transformRushingData rawData'

type PlayersReceivingStats = Map.Map String ReceivingStats

fetchReceivingData :: Int -> IO (Either String PlayersReceivingStats)
fetchReceivingData year = do
  rawData <- fetchData Receiving year
  case rawData of
    Left e -> return $ Left e
    Right rawData' -> return $ transformReceivingData rawData'

transformReceivingData :: RawData -> Either String (Map.Map String ReceivingStats)
transformReceivingData (headers', rows) =
  case getReceivingHeaders headers' of
    Left e -> Left e
    Right headers -> Right $ foldl (insertStats headers) Map.empty (chunkedRows (headers', rows))
  where
    insertStats :: ReceivingHeaders -> (Map.Map String ReceivingStats) -> [String] -> (Map.Map String ReceivingStats)
    insertStats headers acc row =
      Map.insert (strip (row !! (ReceivingHeaders.playerNameI headers))) (stats headers row) acc

    stats :: ReceivingHeaders -> [String] -> ReceivingStats
    stats hs row =
      ReceivingStats
        { receptions = read $ row !! (recI hs),
          receivingYards = read $ row !! (recYdsI hs),
          receivingTouchdowns = read $ row !! (recTdsI hs),
          receivingFumbles = read $ row !! (recFumblesI hs)
        }

type PlayersKickingStats = Map.Map String KickingStats

fetchKickingData :: Int -> IO (Either String PlayersKickingStats)
fetchKickingData year = do
  rawData <- fetchData Kicking year
  case rawData of
    Left e -> return $ Left e
    Right rawData' -> return $ transformKickingData rawData'

transformKickingData :: RawData -> Either String (Map.Map String KickingStats)
transformKickingData (headers', rows) =
  case getKickingHeaders headers' of
    Left e -> Left e
    Right headers -> Right $ foldl (insertStats headers) Map.empty (chunkedRows (headers', rows))
  where
    insertStats :: KickingHeaders -> Map.Map String KickingStats -> [String] -> Map.Map String KickingStats
    insertStats headers acc row =
      Map.insert (strip (row !! KickingHeaders.playerNameI headers)) (stats headers row) acc

    stats :: KickingHeaders -> [String] -> KickingStats
    stats hs row =
      KickingStats
        { fieldGoalsMade0to19 = read $ takeWhile Char.isDigit $ row !! fg0to19I hs,
          fieldGoalsMade20to29 = read $ takeWhile Char.isDigit $ row !! fg20to29I hs,
          fieldGoalsMade30to39 = read $ takeWhile Char.isDigit $ row !! fg30to39I hs,
          fieldGoalsMade40to49 = read $ takeWhile Char.isDigit $ row !! fg40to49I hs,
          fieldGoalsMade50to59 = read $ takeWhile Char.isDigit $ row !! fg50to59I hs,
          fieldGoalsMade60Plus = read $ takeWhile Char.isDigit $ row !! fg60PlusI hs
        }

-- TODO: something something monad transformers EitherIO
fetchPlayerData :: Int -> IO (Either String PlayersStats)
fetchPlayerData year = do
  passingDataE <- fetchPassingData year
  case passingDataE of
    Left e -> return (Left e)
    Right passingData -> do
      rushingDataE <- fetchRushingData year
      case rushingDataE of
        Left e -> return (Left e)
        Right rushingData -> do
          receivingDataE <- fetchReceivingData year
          case receivingDataE of
            Left e -> return (Left e)
            Right receivingData -> do
              kickingDataE <- fetchKickingData year
              case kickingDataE of
                Left e -> return (Left e)
                Right kickingData ->
                  return $ Right $ mergeStats passingData rushingData receivingData kickingData

mergeStats :: PlayersPassingStats -> PlayersRushingStats -> PlayersReceivingStats -> PlayersKickingStats -> PlayersStats
mergeStats passingData rushingData receivingData kickingData =
  foldl foldPlayer Map.empty allNames
  where
    allNames = Set.union (Map.keysSet kickingData) $ Set.union (Map.keysSet receivingData) $ Set.union (Map.keysSet passingData) (Map.keysSet rushingData)

    foldPlayer :: PlayersStats -> String -> PlayersStats
    foldPlayer acc name =
      Map.insert name (makeStatsRecord name) acc

    makeStatsRecord :: String -> PlayerStats
    makeStatsRecord name =
      PlayerStats
        { passing = Map.findWithDefault defaultPassingStats name passingData,
          rushing = Map.findWithDefault defaultRushingStats name rushingData,
          receiving = Map.findWithDefault defaultReceivingStats name receivingData,
          kicking = Map.findWithDefault defaultKickingStats name kickingData
        }
