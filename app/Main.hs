module Main where

import CmdExport (exportSDIOFormat)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import MyLib
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  print args
  let cmd = head args
  case cmd of
    "fetch-category" -> do
      let year = read (args !! 1) :: Int
      let category = read (args !! 2) :: NFLDataCategory
      case category of
        Kicking -> showData $ fetchKickingData year
        Passing -> showData $ fetchPassingData year
        Rushing -> showData $ fetchRushingData year
        Receiving -> showData $ fetchReceivingData year
    "fetch" -> do
      let year = read (args !! 1) :: Int
      playerDataE <- fetchPlayerData year
      case playerDataE of
        Left err -> print err
        Right playerData -> do
          let filepath = "player-data-" <> show year <> ".json"
          writeFile filepath $ B.unpack $ encode playerData
          putStrLn $ "Data wrote successfully to " <> filepath
    "export" -> do
      let year = read (args !! 1) :: Int
      sdioExportE <- exportSDIOFormat year
      case sdioExportE of
        Left err -> print err
        Right _ -> putStrLn "Done."
    _ -> putStrLn "Unknown command, options are 'fetch', 'compare', 'export'"

showData :: Show a => IO (Either String a) -> IO ()
showData io = do
  result <- io
  print result
