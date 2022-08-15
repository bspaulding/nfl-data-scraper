module NFLDataCategory where

data NFLDataCategory = Passing | Rushing | Receiving deriving (Enum, Eq, Show)

toParam :: NFLDataCategory -> String
toParam Passing = "passing"
toParam Rushing = "rushing"
toParam Receiving = "receiving"

sortOrder :: NFLDataCategory -> String
sortOrder Passing = "passingyards"
sortOrder Rushing = "rushingyards"
sortOrder Receiving = "receivingreceptions"

