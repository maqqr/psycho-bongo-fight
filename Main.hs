module Main where

import Data.Array ((!))
import qualified Data.Array as A
import Graphics.Gloss.Interface.IO.Game
import Control.Monad

import qualified Game.Client as C
import qualified Game.GameWorld as G
import qualified Game.Tile as T
import qualified Game.Resources as R
import qualified Game.TypeClasses as TC

{- Simon muistiinpanot:

Siirrä Position, toIsom, fromIsom, jne. omaan tiedostoon?

-}

{- Vanhat testityypit:
data Character = Character
type Squad = [Character]

data Tile = Ground | Wall deriving (Eq, Ord, Show)
type GameMap = Array (Int, Int) Tile
data WorldState = WorldState GameMap Squad Squad
-}

--data Game = Game Resources WorldState


tileWidth :: Float
tileWidth = 64

tileHeight :: Float
tileHeight = 32

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

testmap = concat [ "..#."
                 , "..#."
                 , ".##."
                 , "...." ]

-- | Tarkistaa onko annettu koordinaatti pelikentällä
insideMap :: G.Map -> (Int, Int) -> Bool
insideMap gamemap = A.inRange (A.bounds gamemap)

-- | Muuntaa merkkijonon pelikentäksi
convertMap :: Int     -- ^ Kentän leveys
           -> Int     -- ^ Kentän korkeus
           -> String  -- ^ Kenttä merkkijonona
           -> G.Map
convertMap width height = A.listArray ((0,0), (width-1, height-1)) . map charToTile
    where
        charToTile '.' = T.BasicTile
        charToTile '#' = T.BlockTile
        charToTile _   = T.BasicTile

--initialGame :: ImageRenderer -> Game
--initialGame getImage = Game (Resources getImage (const (return ()))) (WorldState (convertMap 4 4 testmap) [] [])


-- todo: laita tyypiksi Position -> (Float, Float)
-- | Muuntaa pelikoordinaatit isometrisiksi ruutukoordinaateiksi
toIsom :: (Int, Int) -> (Float, Float)
toIsom (x, y) = ((fi y * hw) + (fi x * hw), (fi x * hh) - (fi y * hh))
    where
        hw = tileWidth / 2.0
        hh = tileHeight / 2.0

-- | Muuntaa ruutukoordinaatit pelikoordinaateiksi
fromIsom :: (Float, Float) -> (Int, Int)
fromIsom (x, y) = (floor ((x + 2*y) / tileWidth), floor (-(2*y - x) / tileWidth))

-- | Muuntaa hiiren sijainnin pelikoordinaatiksi
convertMouse :: (Float, Float) -> (Int, Int)
convertMouse (x, y) = let (x', y') = fromIsom (x, y + 64) in (y' + 1, x')


-- | Piirtää pelitilanteen
drawGame :: C.Client -> IO Picture
--drawGame (Game (Resources getImg _) (WorldState gamemap _ _)) = return . scale 1.0 1.0 $ pictures drawTiles
drawGame (C.Client res world) = return $ pictures drawTiles
    where
        drawTiles :: [Picture]
        drawTiles = [uncurry translate (toIsom (x, y)) . drawTile $ (x, y) | x <- [w, w-1 .. 0], y <- [0 .. h]]

        drawTile :: (Int, Int) -> Picture
        drawTile (x, y) = getImg . TC.filename $ gamemap ! (y, x)

        gamemap = G.map world
        getImg  = R.drawImage res
        (w, h) = snd (A.bounds gamemap)


-- | Tapahtumien käsittey
handleEvent :: Event -> C.Client -> IO C.Client
handleEvent (EventMotion mouse) cli@(C.Client _ gameworld) = do
    let m = convertMouse mouse
    print $ show mouse ++ show m
    when (insideMap gamemap m) (print (gamemap ! m))
    return cli
    where
        gamemap = G.map gameworld

handleEvent _ game = return game


main :: IO ()
main = do
    client <- C.newClient
    playIO
        (InWindow "Isometric game" (700, 500) (10, 10))
        white -- background color (Color)
        30    -- fps (Int)
        client  -- initial game state
        drawGame       -- rendering function (game -> IO Picture)
        handleEvent    -- input handler (Event -> game -> IO game)
        (const return) -- update function (Float -> game -> IO game)
