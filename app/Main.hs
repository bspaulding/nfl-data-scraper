module Main where

import CmdCompareSDIO (compareSDIO)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import MyLib (fetchPlayerData)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  print args
  let cmd = head args
  let year = read (args !! 1) :: Int
  case cmd of
    "compare" -> compareSDIO year
    "fetch" -> do
      playerDataE <- fetchPlayerData year
      case playerDataE of
        Left err -> print err
        Right playerData -> do
          let filepath = "player-data-" <> show year <> ".json"
          writeFile filepath $ B.unpack $ encode playerData
          putStrLn $ "Data wrote successfully to " <> filepath
    _ -> putStrLn "Unknown command, options are 'fetch' or 'compare'"
