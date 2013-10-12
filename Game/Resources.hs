{-# LANGUAGE TupleSections #-}
module Game.Resources where

import Control.Monad
import Control.Applicative
import Graphics.Gloss.Interface.IO.Game
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Game.Tile as T
import PngToPic

type ImageFilename = String
type ImageRenderer = T.Tile -> Picture

type Sound = String
type SoundPlayer = Sound -> IO ()

data Resources = Resources {
    drawImage :: ImageRenderer,
    playSound :: SoundPlayer
}

-- | Mäpätään tiilet kuvatiedostoihin
allImages :: M.Map T.Tile ImageFilename
allImages = M.fromList [(T.BasicTile, "ground.png"), (T.BlockTile, "wall.png")]


-- | Lataa pelin kuvat
loadImages :: M.Map T.Tile ImageFilename -> IO (M.Map T.Tile Picture)
loadImages = liftM M.fromList . extractM . M.toList . M.map loadImage
    where
        loadImage :: ImageFilename -> IO Picture
        loadImage = fmap pngToPic . B.readFile

        extractM :: Monad m => [(a, m b)] -> m [(a, b)]
        extractM = mapM (\(a, b) -> liftM (a,) b)


sndplayer :: SoundPlayer
sndplayer = const $ return ()

loadResources :: IO Resources
loadResources = Resources <$> liftM (M.!) (loadImages allImages) <*> return sndplayer
