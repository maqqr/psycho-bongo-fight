{-# LANGUAGE CPP #-}
module Main where

#define SOUND

import Data.Array ((!))
import Data.Maybe (fromMaybe, isNothing, fromJust)
import Data.Binary
import qualified Data.Array as A
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Tuple (swap)
import Graphics.Gloss.Interface.IO.Game
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Network.Simple.TCP

import Game.Position
import qualified Game.Client as C
import qualified Game.GameWorld as G
import qualified Game.Resources as R
import qualified Game.Tile as T
import qualified Game.Unit as U
import qualified Game.TypeClasses as TC
import qualified Game.Actions as A

import qualified Data.Set as S
import Data.Graph.AStar

{- Simon muistiinpanot:

reitinlaskuun:
 [(Int, Position)] polku jossa Int kuvaa paljonko AP jälellä
  jos siihen liikuttaisiin, sen avulla voi visualisoida
  keltaisilla palloilla milloin liikutaan sen verran ettei
  voi lyödä enää, ja vihreillä voi lyödä vielä

-}
pathWithAp :: G.GameWorld -> U.Unit -> [Position] -> [(Int, Position)]
pathWithAp gw u ps = tail . reverse $ foldl f [(U.ap u, U.position u)] ps
  where
    f ls@((ap,_):acc) p = (ap - apCost p, p) : ls
    apCost pos = fromMaybe 10000 . T.tileAp $ G.gamemap gw A.! pos



-- | Etsii reitin pelikentällä kahden pisteen välillä
findPath :: G.GameWorld -- ^ Pelimaailma
         -> Position    -- ^ Aloituspiste
         -> Position    -- ^ Päätepiste
         -> Maybe [Position]
findPath world start end = aStar neighbours distance (heuristicDistance end) (==end) start
    where
        neighbours :: Position -> S.Set Position
        neighbours (px, py) = S.fromList $ filter canMove [newpos | (dx, dy) <- deltaMoves, let newpos = (px+dx, py+dy), G.insideMap gamemap newpos]
            where
                canMove :: Position -> Bool
                -- todo: ei ota huomioon muita yksikköjä, ne pitäs blockaa myös
                canMove pos = not . T.tileBlocking $ gamemap ! pos

                deltaMoves :: [Position]
                deltaMoves = [(1, 0), (-1, 0), (0, -1), (0, 1), (-1, -1), (1, -1), (-1, 1), (1, 1)]

        gamemap = G.gamemap world

        -- | Kahden vierekkäisen pisteen välinen etäisyys
        distance :: Position -> Position -> Int
        distance a b = getAp a + getAp b
            where
                getAp :: Position -> Int
                getAp p = fromMaybe (maxBound :: Int) . T.tileAp $ gamemap ! p

        -- | Pisteen heuristinen etäisyys maaliin
        heuristicDistance :: Position -> Position -> Int
        heuristicDistance (gx, gy) (px, py) = floor $ sqrt (fi (gx-px)**2.0 + fi (gy-py)**2.0 :: Double)


-- | Piirtää pelitilanteen
drawGame :: C.Client -> IO Picture
drawGame client@(C.Client res world mouse selected (sx, sy) self others frameN _ _) =
    return $ pictures [background, translate sx sy (pictures drawTiles), guiElements (G.getUnitAt world mouse), turnInfo]
    where
        background :: Picture
        background = pictures [bgFlasher, bgMagicSquare]
        bgFlasher = color (bgColors L.!! mod frameN (length bgColors)) $ rectangleSolid 750 500
        bgMagicSquare = color (bgBasicColors L.!! mod frameN (length bgBasicColors)) . rotate (fromIntegral $ mod frameN 360) $ rectangleSolid (fromIntegral frameN) (fromIntegral frameN)
        bgColors = blendColors . blendColors . blendColors $ blendColors bgBasicColors -- tähän joku hieno funktio?
        bgBasicColors = [rose, violet, azure, aquamarine, chartreuse, orange]
        bgColor = makeColor8 (mod (frameN + 1) 255) (mod (frameN + 85) 255) (mod (frameN + 170) 255) 255

        blendColors :: [Color] -> [Color]
        blendColors [] = []
        blendColors (c:[]) = [c]
        blendColors (c:d:cs) = c : mixColors 0.5 0.5 c d : blendColors (d:cs)

        drawTiles :: [Picture]
        drawTiles = [uncurry translate (toIsom (x, y)) . drawTile $ (y, x) | x <- [w, w-1 .. 0], y <- [0 .. h]]

        drawTile :: Position -> Picture
        drawTile (x, y) = pictures [tilePicture, cursorPicture, pathPicture, unitPicture]
            where
                tilePicture = getImg . TC.filename $ gamemap ! (x, y)
                pathPicture = case M.lookup (x, y) pointsInPath of
                                Nothing -> Blank
                                Just apLeft -> pathPicByAp apLeft
                pathPicByAp ap
                    | ap > 2 = getImg "greencircle.png"
                    | ap >= 0 = getImg "yellowcircle.png"
                    | otherwise = getImg "redcircle.png"

                cursorPicture
                    | (x, y) == mouse = getImg "cursor.png"
                    | otherwise       = Blank
                unitPicture = case G.getUnitAt world (x, y) of
                    Just unit -> pictures [selectionCursor selected unit, getImg $ TC.filename unit, healthBar unit]
                    Nothing   -> Blank
                selectionCursor :: Maybe U.Unit -> U.Unit -> Picture
                selectionCursor (Just u') u = if u' == u then getImg "cursor.png" else Blank
                selectionCursor Nothing   _ = Blank

                healthBar u = translate 0.0 25.0 . color (healthBarColor u) $ rectangleSolid (U.pp u / 3.0) 5
                healthBarColor u
                    | U.pp u > 50 = green
                    | U.pp u > 20 = yellow
                    | otherwise   = red

        turnInfo :: Picture
        turnInfo = pictures [turnText, turnText2]
            where
                turnText  = translate (-200) 250 . scale 0.3 0.3 . color red . text $ "Turn: " ++ show currentTurn
                turnText2
                    | C.myTurn client = translate (-200) 200 . scale 0.3 0.3 . color green $ text "Your turn"
                    | otherwise       = Blank

        guiElements :: Maybe U.Unit -> Picture
        guiElements Nothing = Blank
        guiElements (Just unit) = translate 300 0 $ pictures [color white $ rectangleSolid 300 600, translate (-140) 250 unitInfo]
            where
                unitInfo :: Picture
                unitInfo = scale 0.3 0.3 . color black . pictures . multiline 0 . lines . TC.describe $ unit

                multiline :: Float -> [String] -> [Picture]
                multiline _ []     = [Blank]
                multiline y (x:xs) = translate 0 y (text x) : multiline (y-160) xs

        currentTurn = G.turn world
        units = G.units world
        gamemap = G.gamemap world
        getImg  = R.drawImage res
        (w, h) = snd (A.bounds gamemap)

        pointsInPath :: M.Map Position Int
        pointsInPath
            | isNothing selected = M.empty
            | otherwise = M.fromList . map swap $ pathWithAp world (fromJust selected) testpath

        testpath :: [Position]
        testpath = fromMaybe [] $ selected >>= \u -> findPath world (U.position u) mouse


-- | Tapahtumien käsittey
handleEvent :: Event -> C.Client -> IO C.Client
handleEvent (EventMotion mouse) client@(C.Client _ gameworld _ _ scroll self others _ _ _) = do
    let m = convertMouse scroll mouse
    --print $ show mouse ++ show m
    --when (G.insideMap gamemap m) (print (gamemap ! m))
    return $ client { C.mousePos = m }
    where
        gamemap = G.gamemap gameworld

-- Klikkaus kun joku yksikkö on valittuna
handleEvent (EventKey (MouseButton LeftButton) Down _ mouse) client@(C.Client _ gameworld _ (Just selection) scroll self others _ _ _) = do
    let m = convertMouse scroll mouse
    --putStrLn $ "Mouse click (unit) " ++ show mouse
    --playSfx client R.BearMove

    let path = fromMaybe [] $ findPath gameworld (U.position selection) m
    let apPath = pathWithAp gameworld selection path
    let unit = selection { U.ap = if L.null apPath then U.ap selection else fst . last $ apPath } -- valittu yksikkö jolta vähennetty AP
    if any (\(ap,_) -> ap < 0) apPath
        then do
              -- todo: soita tööttäysääni
              return client { C.gameworld = gameworld, C.selectedUnit = Nothing }
        else do
              (gw, dead) <- A.action (client { C.gameworld = gameworld }) unit m
              playDeath dead -- todo: piirrä kuolinanimaatio, jos dead ei oo tyhjä
              return client { C.gameworld = gw, C.selectedUnit = Nothing }
    where
        playDeath :: [U.Unit] -> IO ()
        playDeath []    = return ()
        playDeath (x:xs) = do
            void . forkIO $ do
                threadDelay 500000
                playSfx client (U.deathSound x)
            playDeath xs

-- Klikkaus kun mitään hahmoa ei ole valittuna
handleEvent (EventKey (MouseButton LeftButton) Down _ mouse) client@(C.Client _ gameworld _ Nothing scroll self others _ _ _) = do
    let m = convertMouse scroll mouse
    --putStrLn $ "Mouse click (no unit) " ++ show mouse
    case G.getUnitAt gameworld m of
        Just unit -> do
            playSfx client (U.selectSound unit)
            return client { C.selectedUnit = Just unit }
        Nothing -> return client

-- Testaa AP:n palauttamista oletusarvoihin
handleEvent (EventKey (Char 'r') Down _ _) client = return client { C.gameworld = G.resetAps (C.gameworld client) }

-- Vuoron vaihto välilyönnillä
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) client = do
    return client { C.gameworld = newworld }
    where
        newworld = nextTurn . G.resetAps . C.gameworld $ client

        nextTurn :: G.GameWorld -> G.GameWorld
        nextTurn w = w { G.turn = succ (G.turn w) }

-- Scrollaus nuolinäppäimillä
handleEvent (EventKey (SpecialKey key) Down _ _) client = do
    putStrLn $ "asdasd" ++ show key
    return client { C.scroll = newScroll }
    where
        (ox, oy) = C.scroll client
        newScroll = let (dx, dy) = scrolling key in (ox+dx, oy+dy)
        scrolling KeyLeft  = ( 30,   0)
        scrolling KeyRight = (-30,   0)
        scrolling KeyUp    = (  0, -30)
        scrolling KeyDown  = (  0,  30)
        scrolling _        = (  0,   0)

handleEvent _ client = return client


updateGame :: (Float -> C.Client -> IO C.Client)
updateGame dt client = do
    ww <- tryTakeMVar (C.box client)
    let networkworld = case ww of
                    Just w  -> w
                    Nothing -> C.gameworld client
    return client { C.gameworld = G.animateUnits networkworld, C.frame = mod (C.frame client + 1) 1000 }

playSfx :: C.Client -> R.GameSound -> IO ()
playSfx client s = (R.playSound . C.resources $ client) s 1.0 False

sendWorld :: C.Client -> IO ()
sendWorld client =
    send (C.socket client) . BS.pack . LBS.unpack . encode $ C.gameworld client

receiveThread :: Socket -> MVar G.GameWorld -> IO ()
receiveThread sock box = do
    dataa <- recv sock (1024 * 4)
    case dataa of
        Just dataa' -> do
            putStrLn "DATAAA"
            putMVar box (decode . LBS.pack . BS.unpack $ dataa')
            receiveThread sock box
        Nothing -> do
            putStrLn "Connection lost."


main :: IO ()
main = withSocketsDo . R.withSound . connect "www.btlracing.fi" "44444" $ \(sock, addr) -> do
    putStrLn "recv test"
    box <- newEmptyMVar :: IO (MVar G.GameWorld)
    forkIO $ receiveThread sock box
    client <- C.newClient box sock
    playSfx client R.BongoFight
    (R.playSound . C.resources $ client) R.BGMusic 1.0 True
    playIO
        (InWindow "Isometric game" (700, 500) (10, 10))
        black   -- background color (Color)
        30      -- fps (Int)
        client  -- initial game state
        drawGame    -- rendering function (game -> IO Picture)
        handleEvent -- input handler (Event -> game -> IO game)
        updateGame  -- update function (Float -> game -> IO game)
