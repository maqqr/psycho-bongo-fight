module Game.Player where

import Game.Unit (Unit)

data Player = Player { name :: String
                     , unitIndex :: Int } deriving (Show)
