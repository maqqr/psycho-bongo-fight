{-# LANGUAGE RecordWildCards #-}
module Game.GameWorld where

import Game.Position
import Game.Tile (Tile(..))
import Game.Unit
import qualified Data.Array as A
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.List as L
import Control.Applicative
import Data.Binary

type Map = A.Array (Int, Int) Tile 

-- laitoin map :: Map nimeksi gamemap
-- jotta ei tule konfliktia Prelude.mapin kanssa
data GameWorld = GameWorld { gamemap :: Map
                           , units :: [[Unit]]
                           , turn :: Int
                 } deriving (Show)
instance Binary GameWorld where
  put GameWorld{..} = do
    put gamemap
    put units
    put turn
  get = do
    gamemap <- get
    units <- get
    turn <- get
    return GameWorld{..}


initialGameWorld :: IO GameWorld
initialGameWorld = GameWorld
                <$> return mappi
                <*> initialUnits
                <*> return 0
  where
    mappi = convertMap 12 12 testmap

    testmap = concat [ "...o........"
                     , ".#####.Y...."
                     , ".#...#...o.."
                     , ".##.##.O..Y."
                     , ".,,.T......."
                     , ".,,,..Y....."
                     , "===,.....o.."
                     , ",,==........"
                     , ",,O==,..O..."
                     , ".,,======,,,"
                     , "....,,..===="
                     , ".Y....o....o" ]

-- | Luo tyhjän pelikentän
blankMap :: Int -> Int -> Map
blankMap w h = A.listArray ((0,0), (h,w)) (repeat GrassTile)

-- | Muuntaa merkkijonon pelikentäksi
convertMap :: Int     -- ^ Kentän leveys
           -> Int     -- ^ Kentän korkeus
           -> String  -- ^ Kenttä merkkijonona
           -> Map
convertMap width height = A.listArray ((0,0), (width-1, height-1)) . map charToTile
    where
        charToTile '.' = GrassTile
        charToTile '#' = BlockTile
        charToTile 'T' = TreeTile
        charToTile 'Y' = Tree2Tile
        charToTile ',' = MudTile
        charToTile 'o' = RockTile
        charToTile 'O' = Rock2Tile
        charToTile '=' = WaterTile
        charToTile _   = GrassTile

initialUnits :: IO [[Unit]]
initialUnits = mapM sequence [[basicUnit "Matti" (0, 0) 0, basicUnit "Esko" (1, 0) 0, basicUnit "Jussi" (0, 1) 0],
                              [basicUnit "Yarr" (11, 11) 1, basicUnit "Yarhar" (10, 11) 1, basicUnit "Argh" (11, 10) 1]]

-- | Päivittää pelimaailman yksikköä
updateUnit :: GameWorld -> Unit -> GameWorld
updateUnit gw nu = setUnits gw [replaceUnit r | r <- units gw]
  where replaceUnit row = [if u == nu then nu else u | u <- row]

-- | Poistaa yksikön pelimaailmasta
removeUnit :: GameWorld -> Unit -> GameWorld
removeUnit gw ru = setUnits gw [filter (/= ru) r | r <- units gw]

setUnits :: GameWorld -> [[Unit]] -> GameWorld
setUnits (GameWorld m _ t) us = GameWorld m us t

animateUnits :: GameWorld -> GameWorld
animateUnits world = world { units = map (map animateUnit) (units world) }
  where
    animateUnit :: Unit -> Unit
    animateUnit u = u { animFrame = (animFrame u + 1) `mod` maxFrames u }

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

-- | Palauttaa yksiköt, jotka ovat yhden siiron päästä annetusta yksiköstä
getAdjUnits :: GameWorld -> Unit -> [Unit]
getAdjUnits gw u = catMaybes [getUnitAt gw p | p <- getAdjPositions gw (position u)]

getAdjPositions :: GameWorld -> Position -> [Position]
getAdjPositions gw pos =  [(x, y) | (y,x) <- adjs pos, isBetween minX maxX x && isBetween minY maxY y]
  where
    ((minX, minY), (maxX, maxY)) = A.bounds $ gamemap gw
    adjs (y,x) = [(y+y',x+x') | y' <- [-1..1], x' <- [-1..1], (x',y') /= (0,0)]

isBetween :: Ord a => a -> a -> a -> Bool
isBetween lower upper x = x >= lower && x <= upper

-- | Palauttaa kullekin yksikölle oletus-AP:t
resetAps :: GameWorld -> GameWorld
resetAps gw = gw { units = map (\team -> [getAp u | u <- team]) (units gw) }
  where
    getAp u = u { ap = 10 + traitApBonus (traits u) }
    traitApBonus ts = sum [fromMaybe 0 (traitAp t) | t <- ts]
