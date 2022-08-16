module NFLDataCategory where

data NFLDataCategory = Passing | Rushing | Receiving | Kicking deriving (Enum, Eq, Read, Show)

toParam :: NFLDataCategory -> String
toParam Passing = "passing"
toParam Rushing = "rushing"
toParam Receiving = "receiving"
toParam Kicking = "field-goals"

sortOrder :: NFLDataCategory -> String
sortOrder Passing = "passingyards"
sortOrder Rushing = "rushingyards"
sortOrder Receiving = "receivingreceptions"
sortOrder Kicking = "kickingfgmade"
