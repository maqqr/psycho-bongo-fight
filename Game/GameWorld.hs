module GameWorld where

import Tile (Tile(..))
import Unit (Unit, basicUnit)
import qualified Data.Array as A

type Map = A.Array (Int, Int) Tile

data GameWorld = GameWorld { map :: Map
                           , units :: [[Unit]]
                           , turn :: Int
                 } deriving (Show)


initialGameWorld :: GameWorld
initialGameWorld = GameWorld (blankMap 5 5) initialUnits 0

blankMap :: Int -> Int -> Map
blankMap w h = A.listArray ((0,0), (h,w)) (repeat BasicTile)

initialUnits :: [[Unit]]
initialUnits = [[basicUnit "Matti" (0, 0)], [basicUnit "Esko" (1, 1)]]


updateUnit :: GameWorld -> Unit -> GameWorld
updateUnit gw nu = setUnits gw [replaceUnit r | r <- units gw]
  where replaceUnit row = [if u == nu then nu else u | u <- row]

setUnits :: GameWorld -> [[Unit]] -> GameWorld
setUnits (GameWorld m _ t) us = GameWorld m us t
