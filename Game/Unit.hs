module Game.Unit where

import Data.UUID
import Data.UUID.V4
import Game.Position
import Game.TypeClasses
import qualified Game.Resources as R
import Data.Binary

data Unit = Unit { uuid :: UUID
                 , name :: String
                 , ap :: Int
                 , pp :: Float
                 , traits :: [Trait]
                 , position :: Position
                 , team :: Int
                 , animFrame :: Int
            } deriving (Show)

instance Binary Unit where
  put Unit{..} = do
    put uuid
    put name
    put ap
    put pp
    put traits
    put position
    put team
    put animFrame
  get = do
    uuid <- get
    name <- get
    ap <- get
    pp <- get
    traits <- get
    position <- get
    team <- get
    animFrame <- get
    return Unit{..}

instance Eq Unit where
    u1 == u2 = uuid u1 == uuid u2

instance Describable Unit where
    describe u = "Name: "++name u++"\nPP: "++show (pp u)++"\nAP: "++show (ap u)++ unlines (map show (traits u))

instance Drawable Unit where
    filename unit = "characters/"++teamImage unit++show (animFrame unit)++".png"


teamImage :: Unit -> String
teamImage (Unit _ _ _ _ _ _ 0 _) = "bear"
teamImage (Unit _ _ _ _ _ _ 1 _) = "pirate"
teamImage (Unit _ _ _ _ _ _ 2 _) = "ninja"
teamImage (Unit _ _ _ _ _ _ 3 _) = "robot"

maxFrames :: Unit -> Int
maxFrames (Unit _ _ _ _ _ _ 0 _) = 2
maxFrames (Unit _ _ _ _ _ _ 1 _) = 2

moveSound :: Unit -> R.GameSound
moveSound (Unit _ _ _ _ _ _ 0 _) = R.BearMove
moveSound (Unit _ _ _ _ _ _ 1 _) = R.PirateMove
moveSound (Unit _ _ _ _ _ _ 2 _) = R.NinjaMove
moveSound (Unit _ _ _ _ _ _ 3 _) = R.RobotMove


attackSound :: Unit -> R.GameSound
attackSound (Unit _ _ _ _ _ _ 0 _) = R.BearAttack
attackSound (Unit _ _ _ _ _ _ 1 _) = R.PirateAttack
attackSound (Unit _ _ _ _ _ _ 2 _) = R.NinjaAttack
attackSound (Unit _ _ _ _ _ _ 3 _) = R.RobotAttack

selectSound :: Unit -> R.GameSound
selectSound (Unit _ _ _ _ _ _ 0 _) = R.BearSelect
selectSound (Unit _ _ _ _ _ _ 1 _) = R.PirateSelect
selectSound (Unit _ _ _ _ _ _ 2 _) = R.NinjaSelect
selectSound (Unit _ _ _ _ _ _ 3 _) = R.RobotSelect

deathSound :: Unit -> R.GameSound
deathSound (Unit _ _ _ _ _ _ 0 _) = R.BearDie
deathSound (Unit _ _ _ _ _ _ 1 _) = R.PirateDie
deathSound (Unit _ _ _ _ _ _ 2 _) = R.NinjaDeath
deathSound (Unit _ _ _ _ _ _ 3 _) = R.RobotDeath



data TraitType = BasicTrait | StrongTrait | WeakTrait deriving (Show, Eq)

data Trait = Trait { traitAp :: Maybe Int
                   , traitPp :: Unit -> Float
                   , traitType :: TraitType
                   , duration :: Maybe Int }

instance Binary Trait where
  put Trait{..} = do
    put traitAp
    put traitPp
    put traitType
    put duration
  get = do
    traitAp <- get
    traitPp <- get
    traitType <- get
    duration <- get
    return Trait{..}

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

-- | Palauttaa ekan liukuluvun, jos yksiköllä on annettu trait, muuten jälkimmäisen
ifHasTrait :: Unit -> Trait -> Float -> Float -> Float
ifHasTrait u t f1 f2 = if t `elem` traits u then f1 else f2

basicTrait :: Trait
basicTrait = Trait Nothing ppFunc BasicTrait Nothing
  where ppFunc u = ifHasTrait u basicTrait 0.0 10.0

strongTrait :: Trait
strongTrait = Trait Nothing (const 10) StrongTrait Nothing

weakTrait :: Trait
weakTrait = Trait Nothing (const (-10)) WeakTrait Nothing

