module Main where

import Data.Array ((!))
import qualified Data.Array as A
import Graphics.Gloss.Interface.IO.Game
import Control.Monad

import Game.Position
import qualified Game.Client as C
import qualified Game.GameWorld as G
import qualified Game.Tile as T
import qualified Game.Resources as R
import qualified Game.TypeClasses as TC

{- Simon muistiinpanot:

-}

{- Vanhat testityypit:
data Character = Character
type Squad = [Character]

data Tile = Ground | Wall deriving (Eq, Ord, Show)
type GameMap = Array (Int, Int) Tile
data WorldState = WorldState GameMap Squad Squad
-}

--data Game = Game Resources WorldState



--testmap = concat [ "..#."
--                 , "..#."
--                 , ".##."
--                 , "...." ]


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



-- | Piirtää pelitilanteen
drawGame :: C.Client -> IO Picture
--drawGame (Game (Resources getImg _) (WorldState gamemap _ _)) = return . scale 1.0 1.0 $ pictures drawTiles
drawGame (C.Client res world mouse) = return $ pictures drawTiles
    where
        drawTiles :: [Picture]
        drawTiles = [uncurry translate (toIsom (x, y)) . drawTile $ (x, y) | x <- [w, w-1 .. 0], y <- [0 .. h]]

        drawTile :: (Int, Int) -> Picture
        drawTile (x, y) = pictures [tilePicture, cursorPicture]
            where
                tilePicture = getImg . TC.filename $ gamemap ! (y, x)
                cursorPicture
                    | (y, x) == mouse = getImg "cursor.png"
                    | otherwise       = Blank

        gamemap = G.map world
        getImg  = R.drawImage res
        (w, h) = snd (A.bounds gamemap)


-- | Tapahtumien käsittey
handleEvent :: Event -> C.Client -> IO C.Client
handleEvent (EventMotion mouse) cli@(C.Client _ gameworld _) = do
    let m = convertMouse mouse
    print $ show mouse ++ show m
    when (G.insideMap gamemap m) (print (gamemap ! m))
    return $ cli { C.mousePos = m }
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
