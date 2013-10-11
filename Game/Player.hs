module Player where

import Unit (Unit)

data Player = Player { name :: String
                     , units :: [Unit] } deriving (Show)
