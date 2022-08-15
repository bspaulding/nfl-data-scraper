module Main where

import Data.Aeson
import MyLib (fetchPlayerData)
import qualified Data.ByteString.Lazy.Char8 as B
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let year = read (args !! 0) :: Int
  playerDataE <- fetchPlayerData year 
  case playerDataE of
    Left err -> print err
    Right playerData -> do
      let filepath = "player-data-" <> (show year) <> ".json"
      writeFile filepath $ B.unpack $ encode playerData
      putStrLn $ "Data wrote successfully to " <> filepath

