module CmdExport (exportSDIOFormat) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified PlayersStats as PlayersStats
import PlayersStats (PlayersStats)
import qualified PlayerInfo as PlayerInfo
import PlayerInfo (PlayerInfo)
import SDIOPlayerStats

exportSDIOFormat :: Int -> IO (Either String ())
exportSDIOFormat year = do
  playersStats <- readPlayerData year
  let playerInfos = Seq.fromList <$> Map.keys <$> playersStats
  case (playersStats, playerInfos) of
    (Right playersStats', Right playerInfos') -> do
      _ <- writeSdioFormatFile year $ makeSDIOFormat year playersStats' playerInfos'
      return $ Right ()
    (Left e1, Left e2) -> return $ Left $ List.intercalate ". " [e1, e2]
    (Left e1, _) -> return $ Left e1
    (_, Left e2) -> return $ Left e2

readPlayerData :: Int -> IO (Either String PlayersStats)
readPlayerData year = do
  dataStr <- readFile $ "player-data-" <> show year <> ".json"
  return $ eitherDecode $ L8.pack dataStr

makeSDIOFormat :: Int -> PlayersStats -> Seq.Seq PlayerInfo -> Seq.Seq SDIOPlayerStats
makeSDIOFormat year playersStats playerInfos =
  Seq.mapWithIndex (toSDIOPlayerStats year playersStats) playerInfos

toSDIOPlayerStats :: Int -> PlayersStats -> Int -> PlayerInfo -> SDIOPlayerStats
toSDIOPlayerStats year playersStats i playerInfo = SDIOPlayerStats
  { playerID = i,
    season = year,
    name = PlayerInfo.name playerInfo,
    position = PlayerInfo.position playerInfo,
    passingCompletions = fromIntegral $ PlayersStats.passingCompletions playerStats,
    passingYards = fromIntegral $ PlayersStats.passingYards playerStats,
    passingTouchdowns = fromIntegral $ PlayersStats.passingTouchdowns playerStats,
    passingInterceptions = fromIntegral $ PlayersStats.passingInterceptions playerStats,
    rushingYards = fromIntegral $ PlayersStats.rushingYards playerStats,
    rushingTouchdowns = fromIntegral $ PlayersStats.rushingTouchdowns playerStats,
    fumbles = fromIntegral $ PlayersStats.fumbles playerStats,
    receptions = fromIntegral $ PlayersStats.receptions playerStats,
    receivingYards = fromIntegral $ PlayersStats.receivingYards playerStats,
    receivingTouchdowns = fromIntegral $ PlayersStats.receivingTouchdowns playerStats,
    twoPointConversionPasses = 0, -- TODO
    twoPointConversionRuns = 0, -- TODO
    twoPointConversionReceptions = 0, -- TODO
    extraPointsMade = 0, -- TODO
    extraPointsAttempted = 0, -- TODO
    played = 16, -- TODO
    fieldGoalsMade0to19  = fromIntegral $ PlayersStats.fieldGoalsMade0to19 playerStats,
    fieldGoalsMade20to29 = fromIntegral $ PlayersStats.fieldGoalsMade20to29 playerStats,
    fieldGoalsMade30to39 = fromIntegral $ PlayersStats.fieldGoalsMade30to39 playerStats,
    fieldGoalsMade40to49 = fromIntegral $ PlayersStats.fieldGoalsMade40to49 playerStats,
    fieldGoalsMade50Plus = fromIntegral $ PlayersStats.fieldGoalsMade50Plus playerStats
  }
  where
    playerStats = Map.findWithDefault PlayersStats.defaultPlayerStats playerInfo playersStats

writeSdioFormatFile :: Int -> Seq.Seq SDIOPlayerStats -> IO ()
writeSdioFormatFile year stats =
  writeFile filename $ L8.unpack $ encode stats
  where
    filename = "nfl-data-export-" <> show year <> ".json"
