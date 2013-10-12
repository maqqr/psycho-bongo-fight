module Game.Client where

import Control.Applicative
import qualified Game.GameWorld as G
import qualified Game.Resources as R

data Client = Client R.Resources G.GameWorld

newClient :: IO Client
newClient = Client <$> R.loadResources <*> return G.initialGameWorld
