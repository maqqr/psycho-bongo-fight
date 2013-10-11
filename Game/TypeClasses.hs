module TypeClasses where

class Describable a where
    describe :: a -> String
