module Game.TypeClasses where

class Describable a where
    describe :: a -> String

class Drawable a where
    filename :: a -> String
