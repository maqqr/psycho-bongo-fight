module Actions where

import GameWorld (GameWorld, updateUnit)
import Unit (Unit, Position, moveUnit)
import Tile (Tile)

-- | Siirtää yksikön annettuun paikkaan.
-- Huom! Oikeellisuustarkistukset tehdään aiemmin
-- (ei pitäis olla päällekkäisyyksiä)
move :: Unit -> Position -> GameWorld -> GameWorld
move u pos gw = updateUnit gw (moveUnit u pos)

-- | Huitasee ukkelia
smack :: GameWorld -> Unit -> Unit -> GameWorld
smack gw u1 u2 = undefined

getPercentage :: Unit -> Unit -> Tile -> Float
getPercentage att def t = undefined
