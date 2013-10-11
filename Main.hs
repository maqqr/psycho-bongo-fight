{-# LANGUAGE TupleSections #-}
import Data.Char
import Data.Array (Array, (!))
import qualified Data.Array as A
import qualified Data.ByteString as B
import qualified Data.Map as M
import Graphics.Gloss.Interface.IO.Game
import Control.Applicative
import Control.Monad
import PngToPic


data Character = Character
type Squad = [Character]

data Tile = Ground | Wall deriving (Eq, Ord, Show)
type GameMap = Array (Int, Int) Tile
data WorldState = WorldState GameMap Squad Squad


type ImageFilename = String
type ImageRenderer = Tile -> Picture

type Sound = String
type SoundPlayer = Sound -> IO ()

data Resources = Resources ImageRenderer SoundPlayer
data Game = Game Resources WorldState


allImages :: M.Map Tile ImageFilename
allImages = M.fromList [(Ground, "ground.png"), (Wall, "wall.png")]

tileWidth = 64
tileHeight = 32

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

testmap = concat [ "..#."
                 , "..#."
                 , ".##."
                 , "...." ]

insideMap :: GameMap -> (Int, Int) -> Bool
insideMap gamemap = A.inRange (A.bounds gamemap)

convertMap :: Int -> Int -> String -> GameMap
convertMap width height = A.listArray ((0,0), (width-1, height-1)) . map charToTile
    where
        charToTile '.' = Ground
        charToTile '#' = Wall
        charToTile _   = Ground

initialGame :: ImageRenderer -> Game
initialGame getImage = Game (Resources getImage (const (return ()))) (WorldState (convertMap 4 4 testmap) [] [])


-- todo: laita tyypiksi Position -> (Float, Float)
toIsom :: (Int, Int) -> (Float, Float)
toIsom (x, y) = ((fi y * hw) + (fi x * hw), (fi x * hh) - (fi y * hh))
    where
        hw = tileWidth / 2.0
        hh = tileHeight / 2.0

fromIsom :: (Float, Float) -> (Int, Int)
fromIsom (x, y) = (floor ((x + 2*y) / tileWidth), floor (-(2*y - x) / tileWidth))

convertMouse :: (Float, Float) -> (Int, Int)
convertMouse (x, y) = let (x', y') = fromIsom (x, y + 64) in (y' + 1, x')


drawGame :: Game -> IO Picture
drawGame (Game (Resources getImg _) (WorldState gamemap _ _)) = return . scale 1.0 1.0 $ pictures drawTiles
    where
        drawTiles :: [Picture]
        drawTiles = [uncurry translate (toIsom (x, y)) . drawTile $ (x, y) | x <- [w, w-1 .. 0], y <- [0 .. h]]

        drawTile :: (Int, Int) -> Picture
        drawTile (x, y) = getImg $ gamemap ! (y, x)

        (w, h) = snd (A.bounds gamemap)

loadImages :: M.Map Tile ImageFilename -> IO (M.Map Tile Picture)
loadImages = liftM M.fromList . extractM . M.toList . M.map loadImage
    where
        loadImage :: ImageFilename -> IO Picture
        loadImage = fmap pngToPic . B.readFile

        extractM :: Monad m => [(a, m b)] -> m [(a, b)]
        extractM = mapM (\(a, b) -> liftM (a,) b)


handleEvent :: Event -> Game -> IO Game
handleEvent (EventMotion mouse@(x, y)) game@(Game _ (WorldState gamemap _ _)) = do
    let m = convertMouse mouse
    print $ show mouse ++ show m
    when (insideMap gamemap m) (print (gamemap ! m))
    return game

handleEvent _ game = return game


main :: IO ()
main = do
    images <- loadImages allImages

    playIO
        (InWindow "Isometric game" (700, 500) (10, 10))
        white -- background color (Color)
        30    -- fps (Int)
        (initialGame (images M.!))  -- initial game state
        drawGame       -- rendering function (game -> IO Picture)
        handleEvent    -- input handler (Event -> game -> IO game)
        (const return) -- update function (Float -> game -> IO game)
