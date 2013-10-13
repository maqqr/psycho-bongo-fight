module Game.Client where

import Control.Applicative
import Game.Position
import Game.Player
import qualified Game.GameWorld as G
import qualified Game.Resources as R
import qualified Game.Unit as U

-- | Client kuvaa koko asiakasohjelman tilaa
data Client = Client {
    resources :: R.Resources,
    gameworld :: G.GameWorld,
    mousePos  :: Position,
    selectedUnit :: Maybe U.Unit,
    scroll    :: (Float, Float),
    player    :: Player,
    others    :: [Player],
    frame     :: Int
}

-- | Luo uuden clientin ja lataa sille resurssit
newClient :: IO Client
newClient = Client
         <$> R.loadResources
         <*> G.initialGameWorld
         <*> return (0, 0)
         <*> return Nothing
         <*> return (-150, 0)
         <*> return (Player "pelaaja" 0)
         <*> return []
         <*> return 0

