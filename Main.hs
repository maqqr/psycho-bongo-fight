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

import qualified Data.Set as S
import Data.Graph.AStar

{- Simon muistiinpanot:

-}

-- | Etsii reitin pelikentällä kahden pisteen välillä
findPath :: G.GameWorld -- ^ Pelimaailma
         -> Position    -- ^ Aloituspiste
         -> Position    -- ^ Päätepiste
         -> Maybe [Position]
findPath world start end = aStar neighbours distance (heuristicDistance end) (==end) start
    where
        neighbours :: Position -> S.Set Position
        neighbours (px, py) = S.fromList $ filter canMove [newpos | (dx, dy) <- deltaMoves, let newpos = (px+dx, py+dy), G.insideMap (G.gamemap world) newpos]
            where
                canMove :: Position -> Bool
                canMove = const True

                deltaMoves :: [Position]
                deltaMoves = [(1, 0), (-1, 0), (0, -1), (0, 1), (-1, -1), (1, -1), (-1, 1), (1, 1)]

        -- | Kahden vierekkäisen pisteen välinen etäisyys
        distance :: Position -> Position -> Int
        distance _ _ = 1

        -- | Pisteen heuristinen etäisyys maaliin
        heuristicDistance :: Position -> Position -> Int
        heuristicDistance (gx, gy) (px, py) = floor . sqrt . fromIntegral $ (gx-px)^2 + (gy-py)^2



-- | Piirtää pelitilanteen
drawGame :: C.Client -> IO Picture
--drawGame (Game (Resources getImg _) (WorldState gamemap _ _)) = return . scale 1.0 1.0 $ pictures drawTiles
drawGame (C.Client res world mouse) = return $ pictures drawTiles
    where
        drawTiles :: [Picture]
        drawTiles = [uncurry translate (toIsom (x, y)) . drawTile $ (y, x) | x <- [w, w-1 .. 0], y <- [0 .. h]]

        drawTile :: Position -> Picture
        drawTile (x, y) = pictures [tilePicture, cursorPicture]
            where
                tilePicture = getImg . TC.filename $ gamemap ! (x, y)
                cursorPicture
                    | (x, y) == mouse = getImg "cursor.png"
                    | otherwise       = Blank

        gamemap = G.gamemap world
        getImg  = R.drawImage res
        (w, h) = snd (A.bounds gamemap)


-- | Tapahtumien käsittey
handleEvent :: Event -> C.Client -> IO C.Client
handleEvent (EventMotion mouse) client@(C.Client _ gameworld _) = do
    let m = convertMouse mouse
    print $ show mouse ++ show m
    when (G.insideMap gamemap m) (print (gamemap ! m))
    return $ client { C.mousePos = m }
    where
        gamemap = G.gamemap gameworld

handleEvent (EventKey (MouseButton LeftButton) Down modf mouse) client = do
    putStrLn $ "Mouse click " ++ show mouse
    return client

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
