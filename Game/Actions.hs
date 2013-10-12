module Game.Actions where

import GameWorld (GameWorld, updateUnit, getUnitTile)
import Unit (Unit, Position, moveUnit)
import Tile (Tile)


-- | Siirtää yksikön annettuun paikkaan.
-- Huom! Oikeellisuustarkistukset tehdään aiemmin
-- (ei pitäis olla päällekkäisyyksiä)
move :: Unit -> Position -> GameWorld -> GameWorld
move u pos gw = updateUnit gw (moveUnit u pos)

-- | Huitasee ukkelia.
smack :: GameWorld -> Unit -> Unit -> GameWorld
smack gw u1 u2 = undefined
  where (perc1, perc2) = getPercentages u1 u2 gw

getPercentages :: Unit -> Unit -> GameWorld -> (Float, Float)
getPercentages att def gw = undefined
  where
    attTile = getUnitTile gw att
    defTile = getUnitTile gw def
