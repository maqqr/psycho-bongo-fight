module Game.Unit where

import Data.UUID
import Data.UUID.V4
import Game.Position
import qualified Game.Trait as T
import Game.TypeClasses

data Unit = Unit { uuid :: UUID
                 , name :: String
                 , ap :: Int
                 , pp :: Float
                 , traits :: [T.Trait]
                 , position :: Position
                 , team :: Int
                 , animFrame :: Int
            } deriving (Show)

instance Eq Unit where
    u1 == u2 = uuid u1 == uuid u2

instance Describable Unit where
    describe = name

instance Drawable Unit where
    filename unit
        | team unit == 0 = "characters/bear1.png"
        | otherwise      = ""

basicUnit :: String -> Position -> IO Unit
basicUnit n pos = nextRandom >>= \uuid -> return $ Unit uuid n 10 100 [] pos 0 0

moveUnit :: Unit -> Position -> Unit
moveUnit unit newpos = unit { position = newpos }
