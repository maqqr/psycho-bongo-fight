module Game.Client where

import Network.Simple.TCP
import Control.Concurrent.MVar
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
    frame     :: Int,
    box       :: MVar G.GameWorld,
    socket    :: Socket
}

playerNum :: Client -> Int
playerNum client = 1 + length (others client)

myTurn :: Client -> Bool
myTurn client = G.turn (gameworld client) `mod` playerNum client == teamIndex (player client)

myTeam :: Client -> Int
myTeam client = teamIndex (player client)

-- | Luo uuden clientin ja lataa sille resurssit
newClient :: MVar G.GameWorld -> Socket -> Int -> IO Client
newClient box sock idx = Client
         <$> R.loadResources
         <*> G.initialGameWorld
         <*> return (0, 0)
         <*> return Nothing
         <*> return (-150, 0)
         <*> return (Player "pelaaja" idx)
         <*> return [Player "toinen" 3]
         <*> return 0
         <*> return box
         <*> return sock

