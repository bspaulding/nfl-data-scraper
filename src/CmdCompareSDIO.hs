module CmdCompareSDIO where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.List as List
import qualified Data.Map as Map
import PlayersStats
import SDIOPlayerStats

compareSDIO :: Int -> IO ()
compareSDIO year = do
  putStrLn $ "compareSDIO year = " <> show year
  let filePath = "sportsdataio-" <> show year <> "REG.json"
  jsonStr <- B.readFile filePath
  let sdioM = eitherDecode jsonStr :: Either String [SDIOPlayerStats]
  case sdioM of
    Left err -> putStrLn $ "Error parsing sportsdataio file: " <> filePath <> " => " <> err
    Right sdio -> do
      let scrapedFile = "player-data-" <> show year <> ".json"
      scrapedJson <- B.readFile scrapedFile
      let scrapedM = decode scrapedJson :: Maybe PlayersStats
      case scrapedM of
        Nothing -> putStrLn $ "Error parsing scraped file: " <> scrapedFile
        Just scraped -> do
          let sdBradyM = List.find (\p -> name p == "T.Brady") sdio
          case sdBradyM of
            Nothing -> putStrLn "Whoops, couldn't find player!"
            Just sdBrady -> do
              let nflTomM = Map.lookup "Tom Brady" scraped
              case nflTomM of
                Nothing -> putStrLn "Whoops, couldn't find player in our scraped data!"
                Just nflBrady -> do
                  print sdBrady
                  print nflBrady