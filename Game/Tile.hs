module Game.Tile where

import Game.TypeClasses

data Tile = BasicTile | BlockTile deriving (Eq, Ord, Show)

-- aleksille pohdittavaa:
-- mitäs jos tilaAp palauttaskin Maybe Int?
-- Nothing == Ei saa kulkea
-- Just x == Kulku maksaa x verran
-- voisi olla myäs apufunktio
-- tileBlocking :: Tile -> Bool
-- tileBlocking tile = isNothing . tileAp
-- (isNothing löytyy Data.Maybesta)
tileAp :: Tile -> Int
tileAp BasicTile = 1
tileAp BlockTile = 1000000  -- -1??

instance Describable Tile where
    describe BasicTile = "Basic tile"
    describe BlockTile = "Impassable tile"
    describe _ = "Magical mystery tile"

instance Drawable Tile where
	filename BasicTile = "ground.png"
	filename BlockTile = "wall.png"
