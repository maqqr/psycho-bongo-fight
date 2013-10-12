{-# LANGUAGE TupleSections, RankNTypes #-}
module Game.Resources where

import Control.Monad
import Control.Applicative
import Graphics.Gloss.Interface.IO.Game
import qualified Data.Map as M
import qualified Data.ByteString as B
import PngToPic

type ImageFilename = String
type ImageStorage = M.Map ImageFilename Picture
type ImageRenderer = ImageFilename -> Picture

type Sound = String
type SoundPlayer = Sound -> IO ()

data Resources = Resources {
    drawImage :: ImageRenderer,
    playSound :: SoundPlayer
}

-- | Lista ladattavista kuvista
allImages :: [ImageFilename]
allImages = ["ground.png", "wall.png"]

-- | Lataa pelin kuvat
loadImages :: [ImageFilename] -> IO (M.Map ImageFilename Picture)
loadImages names = liftM (M.fromList . zip names) (mapM loadImage names)
    where
        loadImage :: ImageFilename -> IO Picture
        loadImage = fmap pngToPic . B.readFile

sndplayer :: SoundPlayer
sndplayer = const $ return ()

-- | Lataa pelin kaikki resurssit
loadResources :: IO Resources
loadResources = Resources <$> liftM (M.!) (loadImages allImages) <*> return sndplayer
