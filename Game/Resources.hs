{-# LANGUAGE CPP, TupleSections, RankNTypes #-}
module Game.Resources where

#define SOUND

import Control.Monad
import Control.Applicative
import Graphics.Gloss.Interface.IO.Game
import qualified Data.Map as M
import qualified Data.ByteString as B
import PngToPic

#ifdef SOUND
import qualified Sound as Snd
withSound :: IO () -> IO ()
withSound = Snd.withSound Snd.defaultSoundConfig {Snd.path = ""}

#else
import qualified NoSound as Snd
withSound :: IO () -> IO ()
withSound = id
#endif



type ImageFilename = String
type ImageStorage = M.Map ImageFilename Picture
type ImageRenderer = ImageFilename -> Picture

data GameSound = BongoFight
               | BGMusic
               | BearMove
               | BearAttack
               | BearDie
               | BearSelect
               | PirateMove
               | PirateAttack
               | PirateDie
               | PirateSelect

instance Snd.Playable GameSound where
    filename BongoFight = "snd\\psycho-bongo-fight.wav"
    filename BGMusic    = "snd\\bongo-loop.wav"
    filename BearMove   = "snd\\bear-move.wav"
    filename BearAttack = "snd\\bear-attack.wav"
    filename BearDie    = "snd\\bear-killed.wav"
    filename BearSelect = "snd\\bear-select.wav"
    filename PirateMove = "snd\\pirate-move.wav"
    filename PirateAttack = "snd\\pirate-attack.wav"
    filename PirateDie  = "snd\\pirate-killed.wav"
    filename PirateSelect = "snd\\pirate-select.wav"

type SoundPlayer = GameSound -> Float -> Bool -> IO ()

data Resources = Resources {
    drawImage :: ImageRenderer,
    playSound :: SoundPlayer
}

-- | Tiilen leveys pikseleinä
tileWidth :: Float
tileWidth = 64

-- | Tiilen korkeus pikseleinä
tileHeight :: Float
tileHeight = 32

-- | Lista ladattavista kuvista
allImages :: [ImageFilename]
allImages = tiles ++ other ++ characters
	where
    tiles = ["grass.png", "ground.png", "wall.png", "tree.png", "tree2.png", "mud.png", "water.png", "stone2.png", "stone3.png"]
    other = ["cursor.png", "greencircle.png", "yellowcircle.png", "redcircle.png"]
    characters = makeAnimation "bear" 2 ++ makeAnimation "pirate" 2

makeAnimation :: String -> Int -> [String]
makeAnimation name n = ["characters/"++name++show i++".png" | i <- [0..n-1]]

-- | Lataa pelin kuvat
loadImages :: [ImageFilename] -> IO (M.Map ImageFilename Picture)
loadImages names = liftM (M.fromList . zip names) (mapM loadImage names)
    where
        loadImage :: ImageFilename -> IO Picture
        loadImage = fmap pngToPic . B.readFile . (++) "img/"

-- | Lataa pelin kaikki resurssit
loadResources :: IO Resources
loadResources = Resources
             <$> liftM (M.!) (loadImages allImages)
             <*> return (\s vol loop -> void $ Snd.playStream s vol loop)
