module Game.Tile where

import Data.Maybe (isNothing)
import Game.TypeClasses

data Tile = BasicTile | BlockTile deriving (Eq, Ord, Show)

tileAp :: Tile -> Maybe Int
tileAp BasicTile = Just 2
tileAp BlockTile = Nothing

tileBlocking :: Tile -> Bool
tileBlocking = isNothing . tileAp

tileAttBonus :: Tile -> Float
tileAttBonus _ = 0 -- todo eri tiilityypeille eri arvot

tileDefBonus :: Tile -> Float
tileDefBonus _ = 0



instance Describable Tile where
    describe BasicTile = "Basic tile"
    describe BlockTile = "Impassable tile"
    describe _ = "Magical mystery tile"

instance Drawable Tile where
    filename BasicTile = "grass.png"
    filename BlockTile = "wall.png"
