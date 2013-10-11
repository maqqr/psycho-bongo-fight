module Tile where

import TypeClasses (Describable(describe))

data Tile = BasicTile | BlockTile deriving (Show)

tileAp :: Tile -> Int
tileAp BasicTile = 1
tileAp BlockTile = 1000000  -- -1??

instance Describable Tile where
    describe BasicTile = "Basic tile"
    describe BlockTile = "Impassable tile"
    describe _ = "Magical mystery tile"
