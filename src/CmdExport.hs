module CmdExport (exportSDIOFormat) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.List as List
import qualified Data.Map as Map
import qualified PlayersStats as PlayersStats
import PlayersStats (PlayersStats)
import qualified PlayerInfo as PlayerInfo
import PlayerInfo (PlayerInfo)
import SDIOPlayerStats

exportSDIOFormat :: Int -> IO (Either String ())
exportSDIOFormat year = do
  playersStats <- readPlayerData year
  print playersStats
  playerInfos <- readPlayerInfos
  print playerInfos
  case (playersStats, playerInfos) of
    (Right playersStats', Right playerInfos') -> do
      _ <- writeSdioFormatFile year $ makeSDIOFormat playersStats' playerInfos'
      return $ Right ()
    (Left e1, Left e2) -> return $ Left $ List.intercalate ". " [e1, e2]
    (Left e1, _) -> return $ Left e1
    (_, Left e2) -> return $ Left e2

readPlayerData :: Int -> IO (Either String PlayersStats)
readPlayerData year = do
  dataStr <- readFile $ "player-data-" <> show year <> ".json"
  return $ eitherDecode $ L8.pack dataStr

readPlayerInfos :: IO (Either String [PlayerInfo])
readPlayerInfos = do
  dataStr <- readFile $ "player-infos.json"
  return $ eitherDecode $ L8.pack dataStr

makeSDIOFormat :: PlayersStats -> [PlayerInfo] -> [SDIOPlayerStats]
makeSDIOFormat playersStats playerInfos =
  map (toSDIOPlayerStats playersStats) playerInfos

toSDIOPlayerStats :: PlayersStats -> PlayerInfo -> SDIOPlayerStats
toSDIOPlayerStats playersStats playerInfo = SDIOPlayerStats
  { playerID = 0,
    season = 0,
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
    receivingTouchdowns = fromIntegral $ PlayersStats.receivingTouchdowns playerStats
  }
  where
    playerStats = Map.findWithDefault PlayersStats.defaultPlayerStats (PlayerInfo.name playerInfo) playersStats

writeSdioFormatFile :: Int -> [SDIOPlayerStats] -> IO ()
writeSdioFormatFile year stats =
  writeFile filename $ L8.unpack $ encode stats
  where
    filename = "nfl-data-export-" <> show year <> ".json"
