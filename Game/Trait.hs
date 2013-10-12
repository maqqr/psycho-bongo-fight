module Game.Trait where

import Game.TypeClasses (Describable(describe))

data TraitType = BasicTrait | SuperTrait deriving (Eq)

data Trait = Trait { ap :: Maybe Int
                   , pp :: Unit -> Float
                   , traitType :: TraitType
                   , duration :: Maybe Int } deriving (Show, Eq)

instance Describable Trait where
    describe = traitDescription

traitName :: Trait -> String
traitName BasicTrait = "Basic trait"

traitDescription :: Trait -> String
traitDescription BasicTrait = "Perus treitti"
traitDescription _ = "Joku treitti"

applyTrait :: Trait -> Unit -> Float
applyTrait t enemy = undefined

basicTrait :: Trait
basicTrait = Trait Nothing ppFunc BasicTrait Nothing
  where ppFunc u = if any (\t -> traitType t == BasicTrait) (traits u) then 0 else 10
