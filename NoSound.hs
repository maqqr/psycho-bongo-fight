module NoSound where

import Control.Monad
import Control.Applicative

data SoundConfig = SoundConfig {
    path :: String,
    soundDevice :: Int,
    frequency :: Int
} deriving (Eq, Show)

newtype Channel = Channel Int deriving (Eq, Show)

class Playable a where
    filename :: a -> String

playStream :: Playable a => a -> Float -> Bool -> IO Channel
playStream music volume loop = return $ Channel 0
