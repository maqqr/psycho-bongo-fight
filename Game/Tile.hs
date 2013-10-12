module Game.Tile where

import Data.Maybe (isNothing)
import Game.TypeClasses

data Tile = BasicTile | BlockTile deriving (Eq, Ord, Show)

tileAp :: Tile -> Maybe Int
tileAp BasicTile = Just 1
tileAp BlockTile = Nothing

tileBlocking :: Tile -> Bool
tileBlocking = isNothing . tileAp

tileAttBonus :: Tile -> Float
tileAttBonus _ = 0

tileDefBonus :: Tile -> Float
tileDefBonus _ = 0



instance Describable Tile where
    describe BasicTile = "Basic tile"
    describe BlockTile = "Impassable tile"
    describe _ = "Magical mystery tile"

instance Drawable Tile where
	filename BasicTile = "ground.png"
	filename BlockTile = "wall.png"
