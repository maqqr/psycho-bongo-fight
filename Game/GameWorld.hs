module Game.GameWorld where

import Game.Position
import Game.Tile (Tile(..))
import Game.Unit (Unit(..), basicUnit)
import qualified Data.Array as A
import Control.Applicative

type Map = A.Array (Int, Int) Tile

-- laitoin map :: Map nimeksi gamemap
-- jotta ei tule konfliktia Prelude.mapin kanssa
data GameWorld = GameWorld { gamemap :: Map
                           , units :: [[Unit]]
                           , turn :: Int
                 } deriving (Show)


initialGameWorld :: IO GameWorld
initialGameWorld = GameWorld
                <$> return mappi
                <*> initialUnits
                <*> return 0
  where
    mappi = convertMap 6 6 testmap

    testmap = concat [ "......"
                     , "...##."
                     , "....#."
                     , ".####."
                     , "......"
                     , "......" ]

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

initialUnits :: IO [[Unit]]
initialUnits = sequence . map sequence $ [[basicUnit "Matti" (0, 0)], [basicUnit "Esko" (1, 1)]]


updateUnit :: GameWorld -> Unit -> GameWorld
updateUnit gw nu = setUnits gw [replaceUnit r | r <- units gw]
  where replaceUnit row = [if u == nu then nu else u | u <- row]

setUnits :: GameWorld -> [[Unit]] -> GameWorld
setUnits (GameWorld m _ t) us = GameWorld m us t

-- | Hakee yksikön tietyn ehdon perusteella
getUnit :: GameWorld -> (Unit -> Bool) -> Maybe Unit
getUnit world p = toMaybe $ filter p . concat $ units world
  where
    toMaybe :: [a] -> Maybe a
    toMaybe (x:_) = Just x
    toMaybe _     = Nothing

-- | Hakee yksikön tietystä kohdasta kentältä
getUnitAt :: GameWorld -> Position -> Maybe Unit
getUnitAt world pos = getUnit world (\u -> position u == pos)

-- | Tarkistaa onko annettu koordinaatti pelikentällä
insideMap :: Map -> Position -> Bool
insideMap gmap = A.inRange (A.bounds gmap)

-- | Palauttaa annetun ukkelin tiilen
getUnitTile :: GameWorld -> Unit -> Tile
getUnitTile gw u = gamemap gw A.! position u
