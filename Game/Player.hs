module Game.Player where

import Game.Unit (Unit)

data Player = Player { name :: String
                     , units :: [Unit] } deriving (Show)
