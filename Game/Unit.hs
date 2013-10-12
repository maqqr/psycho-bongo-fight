module Game.Unit where

import Game.Position
import qualified Game.Trait as T
import Game.TypeClasses (Describable(describe))


data Unit = Unit { name :: String
                 , ap :: Int
                 , pp :: Float
                 , traits :: [T.Trait]
                 , position :: Position
            } deriving (Show, Eq)

instance Describable Unit where
    describe = name


basicUnit :: String -> Position -> Unit
basicUnit n = Unit n 10 100 []

moveUnit :: Unit -> Position -> Unit
moveUnit (Unit n a p ts _) = Unit n a p ts

