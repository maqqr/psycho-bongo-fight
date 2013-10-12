module Game.Client where

import Control.Applicative
import Game.Position
import qualified Game.GameWorld as G
import qualified Game.Resources as R
import qualified Game.Unit as U

-- | Client kuvaa koko asiakasohjelman tilaa
data Client = Client {
    resources :: R.Resources,
    gameworld :: G.GameWorld,
    mousePos  :: Position,
    selectedUnit :: Maybe U.Unit
}

-- | Luo uuden clientin ja lataa sille resurssit
newClient :: IO Client
newClient = Client
         <$> R.loadResources
         <*> return G.initialGameWorld
         <*> return (0, 0)
         <*> return Nothing
