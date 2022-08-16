module Main where

import CmdCompareSDIO (compareSDIO)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import MyLib
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  print args
  let cmd = head args
  let year = read (args !! 1) :: Int
  case cmd of
    "compare" -> compareSDIO year
    "fetch-category" -> do
      let category = read (args !! 2) :: NFLDataCategory
      case category of
        Kicking -> showData $ fetchKickingData year
        Passing -> showData $ fetchPassingData year
        Rushing -> showData $ fetchRushingData year
        Receiving -> showData $ fetchReceivingData year
    "fetch" -> do
      playerDataE <- fetchPlayerData year
      case playerDataE of
        Left err -> print err
        Right playerData -> do
          let filepath = "player-data-" <> show year <> ".json"
          writeFile filepath $ B.unpack $ encode playerData
          putStrLn $ "Data wrote successfully to " <> filepath
    _ -> putStrLn "Unknown command, options are 'fetch' or 'compare'"

showData :: Show a => IO (Either String a) -> IO ()
showData io = do
  result <- io
  print result