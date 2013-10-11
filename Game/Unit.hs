module Unit where

import qualified Trait as T

type Position = (Int, Int)

data Unit = Unit { name :: String
                 , ap :: Int
                 , pp :: Double
                 , traits :: [T.Trait]
                 , position :: Position
            } deriving (Show)



