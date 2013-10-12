module Game.Client where

import Control.Applicative
import Game.Position
import qualified Game.GameWorld as G
import qualified Game.Resources as R

data Client = Client {
    resources :: R.Resources,
    gameworld :: G.GameWorld,
    mousePos  :: Position
}

newClient :: IO Client
newClient = Client
         <$> R.loadResources
         <*> return G.initialGameWorld
         <*> return (0, 0)
