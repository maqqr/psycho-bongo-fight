module Unit where

import qualified Trait as T
import TypeClasses (Describable(describe))

type Position = (Int, Int)

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

