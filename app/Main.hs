module Main where

import MyLib (fetchPlayerData)

main :: IO ()
main = do
  playerData <- fetchPlayerData 2021 
  print playerData

