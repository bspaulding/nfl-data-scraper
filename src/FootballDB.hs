{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module FootballDB (fetchPlayers, PlayerInfo(..)) where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Internal
import qualified Data.Char as Char
import qualified Data.Either as Either
import qualified Data.List as List
import Network.HTTP.Simple
import RawData
import Text.XML.HXT.Core
import PlayerInfo

letters :: [Char]
letters = map Char.chr $ take 26 [65..]

fetchPlayers :: IO (Either String [PlayerInfo])
fetchPlayers = do
  pages <- fetchAllPlayerInfoPages
  let (lefts, rights) = Either.partitionEithers pages
  if length lefts > 0
     then return $ Left $ List.intercalate ". " lefts
     else return $ Right (concat rights)

fetchAllPlayerInfoPages :: IO [Either String [PlayerInfo]]
fetchAllPlayerInfoPages =
  fmap concat $ sequence $ map fetchPlayerInfosForLetter letters

fetchPlayerInfosForLetter :: Char -> IO [(Either String [PlayerInfo])]
fetchPlayerInfosForLetter letter = do
  (firstPageE, links) <- fetchPlayerInfosForLetterAndPage letter 1
  rest <- sequence $ map (fetchPlayerInfosForLetterAndPage letter) $ [2..(length links)]
  let pageEs = map fst rest
  return $ [firstPageE] ++ pageEs

fetchPlayerInfosForLetterAndPage :: Char -> Int -> IO ((Either String [PlayerInfo], [String]))
fetchPlayerInfosForLetterAndPage letter page = do
  let url = "https://www.footballdb.com/players/players.html?letter=" <> [letter] <> "&page=" <> show page
  putStrLn $ "Fetching url = " <> url
  request <- parseRequest ("GET " <> url)
  response <- httpLBS $ setRequestHeader "User-Agent" [userAgent] $ setRequestSecure True $ request

  let body = getResponseBody response
  let bodyStr = L8.unpack body
  let doc = readString [withParseHTML yes, withWarnings no] bodyStr
  headers <- runX $ doc //> hasName "thead" //> hasName "th" //> getText
  rows <- runX $ doc //> hasName "tbody" //> hasName "td" //> getText
  links <- runX $ doc
    //> hasAttrValue "class" (== "rptcontrols")
    //> hasName "ul"
    //> hasName "li"
    /> hasName "a"
    >>> getAttrValue "href"

  return (toPlayerInfos ([headers], rows), links)

toPlayerInfos :: RawData -> Either String [PlayerInfo]
toPlayerInfos rawData =
  if length lefts > 0
     then Left $ List.intercalate ". " lefts
     else Right rights
  where
    eithers = map toPlayerInfo $ chunkedRows rawData
    (lefts, rights)= Either.partitionEithers eithers

toPlayerInfo :: [String] -> Either String PlayerInfo
toPlayerInfo row =
  if length row < 2
    then Left $ "Cannot parse PlayerInfo from row: '" <> show row <> "'"
    else Right $ PlayerInfo {name, position = pos}
  where
    (lastName, firstName') = span (/= ',') $ row !! 0
    name = drop 2 firstName' <> " " <> lastName
    pos = row !! 1



userAgent :: Data.ByteString.Internal.ByteString
userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 12_5_1) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/15.6 Safari/605.1.15"
