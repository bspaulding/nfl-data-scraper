module Main where

import CmdExport (exportSDIOFormat)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import MyLib
import System.Environment
import ScraperArgs

main :: IO ()
main = parseScraperArgs >>= run

run :: ScraperArgs -> IO ()
run (ScraperArgs cmd year outputPath statsFilePath) =
  case cmd of
    -- "fetch-category" -> do
    --   let category = read (args !! 2) :: NFLDataCategory
    --   case category of
    --     Kicking -> showData $ fetchKickingData year
    --     Passing -> showData $ fetchPassingData year
    --     Rushing -> showData $ fetchRushingData year
    --     Receiving -> showData $ fetchReceivingData year
    "fetch" -> do
      playerDataE <- fetchPlayerData year
      case playerDataE of
        Left err -> print err
        Right playerData -> do
          let filepath = if null outputPath then "player-data-" <> show year <> ".json" else outputPath
          writeFile filepath $ B.unpack $ encode playerData
          putStrLn $ "Data wrote successfully to " <> filepath
    "export" -> do
      sdioExportE <- exportSDIOFormat year outputPath statsFilePath
      case sdioExportE of
        Left err -> print err
        Right _ -> putStrLn "Done."
    _ -> putStrLn "Unknown command, options are 'fetch', 'export'"

showData :: Show a => IO (Either String a) -> IO ()
showData io = do
  result <- io
  print result
