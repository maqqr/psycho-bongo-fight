module GameWorld where

import Tile (Tile)
import Unit (Unit)
import qualified Data.Array as A

type Map = A.Array (Int, Int) Tile

data GameWorld = GameWorld { map :: Map
                           , units :: [[Unit]]
                           , turn :: Int
                 } deriving (Show)
