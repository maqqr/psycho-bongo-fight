module Game.Tile where

import Data.Binary
import Data.Maybe (isNothing)
import Game.TypeClasses

data Tile = BlockTile
          | GrassTile
          | TreeTile
          | Tree2Tile
          | RockTile
          | Rock2Tile
          | WaterTile
          | MudTile deriving (Eq, Ord, Show, Enum)

instance Binary Tile where
    put = putWord8 . fromIntegral . fromEnum
    get = getWord8 >>= \t -> return . toEnum $ fromIntegral t

tileAp :: Tile -> Maybe Int
tileAp BlockTile = Nothing
tileAp GrassTile = Just 2
tileAp MudTile   = Just 3
tileAp RockTile  = Just 4
tileAp Rock2Tile = Just 4
tileAp WaterTile = Just 4
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
    filename Tree2Tile = "tree2.png"
    filename MudTile   = "mud.png"
    filename RockTile  = "stone2.png"
    filename Rock2Tile = "stone3.png"
    filename WaterTile = "water.png"
