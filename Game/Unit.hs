module Game.Unit where

import Data.UUID
import Data.UUID.V4
import Game.Position
import Game.TypeClasses

data Unit = Unit { uuid :: UUID
                 , name :: String
                 , ap :: Int
                 , pp :: Float
                 , traits :: [Trait]
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


data TraitType = BasicTrait | SuperTrait deriving (Show, Eq)

data Trait = Trait { traitAp :: Maybe Int
                   , traitPp :: Unit -> Float
                   , traitType :: TraitType
                   , duration :: Maybe Int }

instance Show Trait where
    show = traitName

instance Describable Trait where
    describe = traitDescription


basicUnit :: String -> Position -> IO Unit
basicUnit n pos = nextRandom >>= \uuid -> return $ Unit uuid n 10 100 [] pos 0 0

moveUnit :: Unit -> Position -> Unit
moveUnit unit newpos = unit { position = newpos }


traitName :: Trait -> String
traitName _ = "Other trait"

traitDescription :: Trait -> String
traitDescription _ = "Joku treitti"

applyCombatTrait :: Trait -> Unit -> Float
applyCombatTrait t enemy = undefined

basicTrait :: Trait
basicTrait = Trait Nothing ppFunc BasicTrait Nothing
  where ppFunc u = if any (\t -> traitType t == BasicTrait) (traits u) then 0 else 10
