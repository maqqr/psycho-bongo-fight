module Game.Unit where

import Data.UUID
import Data.UUID.V4
import Game.Position
import Game.TypeClasses
import qualified Game.Resources as R

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
    filename unit = "characters/"++teamImage unit++show (animFrame unit)++".png"


teamImage :: Unit -> String
teamImage (Unit _ _ _ _ _ _ 0 _) = "bear"
teamImage (Unit _ _ _ _ _ _ 1 _) = "pirate"
teamImage (Unit _ _ _ _ _ _ 2 _) = "ninja"

maxFrames :: Unit -> Int
maxFrames (Unit _ _ _ _ _ _ 0 _) = 2
maxFrames (Unit _ _ _ _ _ _ 1 _) = 2

moveSound :: Unit -> R.GameSound
moveSound (Unit _ _ _ _ _ _ 0 _) = R.BearMove
moveSound (Unit _ _ _ _ _ _ 1 _) = R.PirateMove

attackSound :: Unit -> R.GameSound
attackSound (Unit _ _ _ _ _ _ 0 _) = R.BearAttack
attackSound (Unit _ _ _ _ _ _ 1 _) = R.PirateAttack

selectSound :: Unit -> R.GameSound
selectSound (Unit _ _ _ _ _ _ 0 _) = R.BearSelect
selectSound (Unit _ _ _ _ _ _ 1 _) = R.PirateSelect

deathSound :: Unit -> R.GameSound
deathSound (Unit _ _ _ _ _ _ 0 _) = R.BearDie
deathSound (Unit _ _ _ _ _ _ 1 _) = R.PirateDie


data TraitType = BasicTrait | SlashWeapon | StabWeapon | BluntWeapon | LeatherArmor | MailArmor | PlateArmor deriving (Show, Eq)

data Trait = Trait { traitAp :: Maybe Int
                   , traitPp :: Unit -> Float
                   , traitType :: TraitType
                   , duration :: Maybe Int }

instance Show Trait where
    show = traitName

instance Describable Trait where
    describe = traitDescription

instance Eq Trait where
    t1 == t2 = traitType t1 == traitType t2


basicUnit :: String -> Position -> Int -> IO Unit
basicUnit n pos team = nextRandom >>= \uuid -> return $ Unit uuid n 10 100 [] pos team 0

moveUnit :: Unit -> Position -> Unit
moveUnit unit newpos = unit { position = newpos }


traitName :: Trait -> String
traitName _ = "Other trait"

traitDescription :: Trait -> String
traitDescription _ = "Joku treitti"

applyCombatTrait :: Trait -> Unit -> Float
applyCombatTrait = traitPp

basicTrait :: Trait
basicTrait = Trait Nothing ppFunc BasicTrait Nothing
  where ppFunc u = if any (\t -> traitType t == BasicTrait) (traits u) then 0 else 10

-- | Palauttaa ekan liukuluvun, jos yksiköllä on annettu trait, muuten jälkimmäisen
ifHasTrait :: Unit -> Trait -> Float -> Float -> Float
ifHasTrait u t f1 f2 = if t `elem` traits u then f1 else f2
