module RawData where

-- First are the table headers, then the concat'd rows from each table
type RawData = ([[String]], [String])

chunkedRows :: RawData -> [[String]]
chunkedRows (headers', rows) = (chunksOf ((length . head) headers') rows)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = (take n xs) : (chunksOf n (drop n xs))


