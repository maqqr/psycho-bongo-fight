module Game.Unit where

import Game.Position
import qualified Game.Trait as T
import Game.TypeClasses


data Unit = Unit { name :: String
                 , ap :: Int
                 , pp :: Float
                 , traits :: [T.Trait]
                 , position :: Position
                 , team :: Int
                 , animFrame :: Int
            } deriving (Show, Eq)

instance Describable Unit where
    describe = name

instance Drawable Unit where
	filename unit
		| team unit == 0 = "characters/bear.png"
		| otherwise      = ""

basicUnit :: String -> Position -> Unit
basicUnit n pos = Unit n 10 100 [] pos 0 0

moveUnit :: Unit -> Position -> Unit
moveUnit unit newpos = unit { position = newpos }
