module Game.GameWorld where

import Game.Position
import Game.Tile (Tile(..))
import Game.Unit (Unit, basicUnit)
import qualified Data.Array as A

type Map = A.Array (Int, Int) Tile

-- laitoin map :: Map nimeksi gamemap
-- jotta ei tule konfliktia Prelude.mapin kanssa
data GameWorld = GameWorld { gamemap :: Map
                           , units :: [[Unit]]
                           , turn :: Int
                 } deriving (Show)


initialGameWorld :: GameWorld
initialGameWorld = GameWorld (blankMap 5 5) initialUnits 0

-- | Luo tyhjän pelikentän
blankMap :: Int -> Int -> Map
blankMap w h = A.listArray ((0,0), (h,w)) (repeat BasicTile)

-- | Muuntaa merkkijonon pelikentäksi
convertMap :: Int     -- ^ Kentän leveys
           -> Int     -- ^ Kentän korkeus
           -> String  -- ^ Kenttä merkkijonona
           -> Map
convertMap width height = A.listArray ((0,0), (width-1, height-1)) . map charToTile
    where
        charToTile '.' = BasicTile
        charToTile '#' = BlockTile
        charToTile _   = BasicTile

initialUnits :: [[Unit]]
initialUnits = [[basicUnit "Matti" (0, 0)], [basicUnit "Esko" (1, 1)]]


updateUnit :: GameWorld -> Unit -> GameWorld
updateUnit gw nu = setUnits gw [replaceUnit r | r <- units gw]
  where replaceUnit row = [if u == nu then nu else u | u <- row]

setUnits :: GameWorld -> [[Unit]] -> GameWorld
setUnits (GameWorld m _ t) us = GameWorld m us t

-- | Tarkistaa onko annettu koordinaatti pelikentällä
insideMap :: Map -> Position -> Bool
insideMap gmap = A.inRange (A.bounds gmap)

-- | Palauttaa annetun ukkelin tiilen
getUnitTile :: GameWorld -> Unit -> Tile
getUnitTile gw u = gamemap gw ! position u
