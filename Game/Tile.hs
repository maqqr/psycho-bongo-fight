module Tile where

import TypeClasses (Describable(describe))

data Tile = BasicTile | BlockTile deriving (Show)


instance Describable Tile where
    describe BasicTile = "Basic tile"
    describe BlockTile = "Impassable tile"
    describe _ = "Magical mystery tile"
