{-# LANGUAGE RecordWildCards #-}

module MyLib (fetchPassingData, fetchRushingData, fetchReceivingData, fetchKickingData, fetchPlayerData, NFLDataCategory (..)) where

import GHC.Conc (getNumCapabilities)
import Control.Concurrent.MSem
import qualified Control.Concurrent.Async as Async
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Char as Char
import Data.Foldable (toList)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Traversable as TR
import Text.Read (readMaybe)
import KickingHeaders
import KickingStats
import NFLDataCategory
import NFLUrlParams
import Network.HTTP.Simple
import PassingHeaders (PassingHeaders (..), getPassingHeaders)
import PassingStats
import PlayerInfoHeaders (PlayerInfoHeaders (..), getPlayerInfoHeaders)
import PlayerInfo (PlayerInfo (..))
import PlayersStats
import RawData
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

zipPlayerInfoAndRow :: PlayerInfo -> [String] -> [String]
zipPlayerInfoAndRow playerInfo row = [playerId playerInfo, position playerInfo, team playerInfo] ++ row

parSequence :: [IO a] -> IO [a]
parSequence xs = do
  cpus <- getNumCapabilities
  sem <- new cpus
  Async.mapConcurrently (with sem . id) xs

mapPool :: TR.Traversable t => Int -> (a -> IO b) -> t a -> IO (t b)
mapPool max f xs = do
    sem <- new max
    Async.mapConcurrently (with sem . f) xs

-- (header cells, row cells, next page link)
fetchPage :: String -> IO ([String], [String], [String])
fetchPage url = do
  putStrLn $ "Fetching url = " <> url
  request <- parseRequest ("GET " <> url)
  response <- httpLBS request
  let body = getResponseBody response
  let bodyStr = L8.unpack body
  -- putStrLn $ "bodyStr = \n" ++ bodyStr
  let doc = readString [withParseHTML yes, withWarnings no] bodyStr
  headersRaw <- runX $ doc //> hasName "thead" //> hasName "th" //> getText
  rowsRaw <- runX $ doc //> hasName "tbody" //> hasName "td" //> getText
  playerInfoLinks <- runX $ doc //> hasName "tbody" //> hasName "td" //> hasName "a" >>> getAttrValue "href"
  let headers = filter (\s -> length s /= 0) $ map strip headersRaw
  let rows = filter (\s -> length s /= 0) $ map strip rowsRaw

  playerInfos <- parSequence (map fetchPlayerInfo playerInfoLinks)

  let headers' = ["playerId", "position", "team"] ++ headers
  let rows' = concat $ zipWith zipPlayerInfoAndRow playerInfos (chunkedRows ([headers], rows))

  nextPageLink <- runX $ doc //> hasAttrValue "class" (== "nfl-o-table-pagination__next") >>> getAttrValue "href"
  if length headers == 0 || length rows == 0
     then return ([], [], [])
     else if ((length rows) `mod` (length headers)) /= 0
       then return (headers', filterEmptyRows (length headers') rows', [])
       else return (headers', rows', nextPageLink)

hasClass :: String -> String -> Bool
hasClass needle haystack =
  any (== (T.pack needle)) $ T.split (== ' ') (T.pack haystack)

fetchPlayerInfo :: String -> IO PlayerInfo
fetchPlayerInfo playerInfoLink = do
  let url = "https://www.nfl.com" ++ playerInfoLink
  putStrLn $ "Fetching player info from " <> url
  request <- parseRequest ("GET " <> url)
  response <- httpLBS request
  let body = getResponseBody response
  let bodyStr = L8.unpack body
  let doc = readString [withParseHTML yes, withWarnings no] bodyStr
  name <- runX $ doc //> hasAttrValue "class" (hasClass "nfl-c-player-header__title") //> getText
  position <- runX $ doc //> hasAttrValue "class" (hasClass "nfl-c-player-header__position") //> getText
  team <- runX $ doc //> hasAttrValue "class" (hasClass "nfl-c-player-header__team") //> getText
  return $ PlayerInfo { PlayerInfo.playerId = playerInfoLink
                      , name = (strip (head name))
                      , position = (strip (head position))
                      , team = if null team then "" else (strip (head team))
                      }

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
checkHeaders (headers', rows) =
  if any (/= (head headers)) (tail headers)
    then Left ("Headers were not consistent! " <> show headers)
    else Right (headers, rows)
  where headers = filter (\xs -> length xs /= 0) headers'

getPlayerInfo :: PlayerInfoHeaders -> [String] -> PlayerInfo
getPlayerInfo headers row =
  PlayerInfo { playerId = row !! PlayerInfoHeaders.playerIdI headers
             , team = row !! PlayerInfoHeaders.teamI headers
             , position = row !! PlayerInfoHeaders.positionI headers
             , name = row !! PlayerInfoHeaders.playerNameI headers
             }

-- transformData
--    :: ([[String]] -> Either String statsHeaders)
--    -> (statsHeaders -> [String] -> stats)
--    -> RawData
--    -> Either String (Map.Map PlayerInfo stats)
transformData getStatsHeaders getStats (headers', rows) =
  case (getPlayerInfoHeaders headers', getStatsHeaders headers') of
    (Left e, _) -> Left e
    (_, Left e) -> Left e
    (Right playerInfoHeaders, Right headers) -> Right $ foldl (insertStats (playerInfoHeaders, headers)) Map.empty (chunkedRows (headers', rows))
  where
    -- insertStats :: (PlayerInfoHeaders, statsHeaders) -> (Map.Map PlayerInfo stats) -> [String] -> (Map.Map PlayerInfo stats)
    insertStats (playerInfoHeaders, headers) acc row =
      Map.insert playerInfo (getStats headers row) acc
      where playerInfo = getPlayerInfo playerInfoHeaders row

getPassingStats :: PassingHeaders -> [String] -> PassingStats
getPassingStats hs row =
  PassingStats
    { passingYards = read $ row !! (passYdsI hs),
      passAttempts = read $ row !! (passAttemptsI hs),
      passingTouchdowns = read $ row !! (passTdsI hs),
      interceptions = read $ row !! (intsI hs),
      completions = read $ row !! (completionsI hs)
    }

getRushingStats :: RushingHeaders -> [String] -> RushingStats
getRushingStats hs row =
  RushingStats
    { rushingYards = read $ row !! (rushYdsI hs),
      rushingAttempts = read $ row !! (rushAttemptsI hs),
      rushingTouchdowns = read $ row !! (rushTdsI hs),
      rushingFumbles = read $ row !! (rushFumblesI hs)
    }

getReceivingStats :: ReceivingHeaders -> [String] -> ReceivingStats
getReceivingStats hs row =
  ReceivingStats
    { receptions = read $ row !! (recI hs),
      receivingYards = read $ row !! (recYdsI hs),
      receivingTouchdowns = read $ row !! (recTdsI hs),
      receivingFumbles = read $ row !! (recFumblesI hs)
    }

getKickingStats :: KickingHeaders -> [String] -> KickingStats
getKickingStats hs row =
  KickingStats
    { fieldGoalsMade0to19 = maybe (-1) id $ readMaybe $ takeWhile Char.isDigit $ row !! fg0to19I hs,
      fieldGoalsMade20to29 = maybe (-1) id $ readMaybe $ takeWhile Char.isDigit $ row !! fg20to29I hs,
      fieldGoalsMade30to39 = maybe (-1) id $ readMaybe $ takeWhile Char.isDigit $ row !! fg30to39I hs,
      fieldGoalsMade40to49 = maybe (-1) id $ readMaybe $ takeWhile Char.isDigit $ row !! fg40to49I hs,
      fieldGoalsMade50to59 = maybe (-1) id $ readMaybe $ takeWhile Char.isDigit $ row !! fg50to59I hs,
      fieldGoalsMade60Plus = maybe (-1) id $ readMaybe$ takeWhile Char.isDigit $ row !! fg60PlusI hs
    }

transformPassingData :: RawData -> Either String PlayersPassingStats
transformPassingData = transformData getPassingHeaders getPassingStats

transformRushingData :: RawData -> Either String PlayersRushingStats
transformRushingData = transformData getRushingHeaders getRushingStats

transformReceivingData :: RawData -> Either String PlayersReceivingStats
transformReceivingData = transformData getReceivingHeaders getReceivingStats

transformKickingData :: RawData -> Either String PlayersKickingStats
transformKickingData = transformData getKickingHeaders getKickingStats

strip :: String -> String
strip = T.unpack . T.strip . T.pack

fetchData :: NFLDataCategory -> Int -> IO (Either String RawData)
fetchData category year = do
  let url = nflUrl (NFLUrlParams {year = year, category = category})
  result <- fetchAllPages url
  -- print result
  return $ checkHeaders result

type PlayersPassingStats = Map.Map PlayerInfo PassingStats

fetchPassingData :: Int -> IO (Either String PlayersPassingStats)
fetchPassingData year = do
  rawData <- fetchData Passing year
  case rawData of
    Left e -> return $ Left e
    Right rawData' -> return $ transformPassingData rawData'

type PlayersRushingStats = Map.Map PlayerInfo RushingStats

fetchRushingData :: Int -> IO (Either String PlayersRushingStats)
fetchRushingData year = do
  rawData <- fetchData Rushing year
  case rawData of
    Left e -> return $ Left e
    Right rawData' -> return $ transformRushingData rawData'

type PlayersReceivingStats = Map.Map PlayerInfo ReceivingStats

fetchReceivingData :: Int -> IO (Either String PlayersReceivingStats)
fetchReceivingData year = do
  rawData <- fetchData Receiving year
  case rawData of
    Left e -> return $ Left e
    Right rawData' -> return $ transformReceivingData rawData'

type PlayersKickingStats = Map.Map PlayerInfo KickingStats

fetchKickingData :: Int -> IO (Either String PlayersKickingStats)
fetchKickingData year = do
  rawData <- fetchData Kicking year
  case rawData of
    Left e -> return $ Left e
    Right rawData' -> do
      return $ transformKickingData rawData'


-- TODO: something something monad transformers EitherIO
fetchPlayerData :: Int -> IO (Either String PlayersStats)
fetchPlayerData year = do
  (passingDataE, rushingDataE, receivingDataE, kickingDataE)
    <- Async.runConcurrently $ (,,,)
    <$> Async.Concurrently (fetchPassingData year)
    <*> Async.Concurrently (fetchRushingData year)
    <*> Async.Concurrently (fetchReceivingData year)
    <*> Async.Concurrently (fetchKickingData year)
  case (passingDataE, rushingDataE, receivingDataE, kickingDataE) of
    (Left e, _, _, _) -> return (Left e)
    (_, Left e, _, _) -> return (Left e)
    (_, _, Left e, _) -> return (Left e)
    (_, _, _, Left e) -> return (Left e)
    (Right passingData, Right rushingData, Right receivingData, Right kickingData) ->
      return $ Right $ mergeStats passingData rushingData receivingData kickingData

mergeStats :: PlayersPassingStats -> PlayersRushingStats -> PlayersReceivingStats -> PlayersKickingStats -> PlayersStats
mergeStats passingData rushingData receivingData kickingData =
  foldl foldPlayer Map.empty allNames
  where
    allNames = Set.union (Map.keysSet kickingData) $ Set.union (Map.keysSet receivingData) $ Set.union (Map.keysSet passingData) (Map.keysSet rushingData)

    foldPlayer :: PlayersStats -> PlayerInfo -> PlayersStats
    foldPlayer acc name =
      Map.insert name (makeStatsRecord name) acc

    makeStatsRecord :: PlayerInfo -> PlayerStats
    makeStatsRecord info =
      PlayerStats
        { passing = Map.findWithDefault defaultPassingStats info passingData,
          rushing = Map.findWithDefault defaultRushingStats info rushingData,
          receiving = Map.findWithDefault defaultReceivingStats info receivingData,
          kicking = Map.findWithDefault defaultKickingStats info kickingData
        }
