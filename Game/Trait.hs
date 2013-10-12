module Game.Trait where

import Game.TypeClasses (Describable(describe))

data Trait = Trait { ap :: Int
                   , pp :: Float
                   , name :: String
                   , description :: String
                   , duration :: Int } deriving (Show, Eq)

instance Describable Trait where
    describe = description
