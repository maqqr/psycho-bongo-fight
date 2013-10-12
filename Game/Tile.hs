module Game.Tile where

import Data.Maybe (isNothing)
import Game.TypeClasses

data Tile = BlockTile | GrassTile | TreeTile | MudTile deriving (Eq, Ord, Show)

tileAp :: Tile -> Maybe Int
tileAp BasicTile = Just 2
tileAp BlockTile = Nothing
tileAp GrassTile = Just 2
tileAp MudTile   = Just 3
tileAp _ = Nothing

tileBlocking :: Tile -> Bool
tileBlocking = isNothing . tileAp

tileAttBonus :: Tile -> Float
tileAttBonus _ = 0 -- todo eri tiilityypeille eri arvot

tileDefBonus :: Tile -> Float
tileDefBonus _ = 0


instance Describable Tile where
    describe GrassTile = "Basic tile"
    describe BlockTile = "Impassable tile"
    describe _ = "Magical mystery tile"

instance Drawable Tile where
    filename GrassTile = "grass.png"
    filename BlockTile = "wall.png"
    filename TreeTile  = "tree.png"
    filename MudTile   = "mud.png"

