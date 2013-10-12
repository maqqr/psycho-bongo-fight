module Game.Actions where

import GameWorld (GameWorld, updateUnit, getUnitTile, getAdjUnits)
import Unit (Unit, Position, moveUnit)
import Tile (Tile, tileAttBonus, tileDefBonus)
import System.Random (randomRIO)


-- | Siirtää yksikön annettuun paikkaan.
-- Huom! Oikeellisuustarkistukset tehdään aiemmin
-- (ei pitäis olla päällekkäisyyksiä)
move :: Unit -> Position -> GameWorld -> GameWorld
move u pos gw = updateUnit gw (moveUnit u pos)

-- | Huitasee ukkelia - palauttaa pelimaailman, sekä listan kuolleista yksiköistä.
smack :: GameWorld -> Unit -> Unit -> IO (GameWorld, [Unit])
smack gw att def = do
    (attBonus, defBonus) <- getDamageBonus gw att def
    (attDmg, defDmg) <- (dmg attBonus (pp att), dmg defBonus (pp def))
    let (gw', dead) = applyDamage gw att defDmg
    let (gw'', dead') = applyDamage gw' def attDmg
    return (gw, dead ++ dead')
  where dmg bonus pp = randomRIO (0.5,1.0) >>= \r -> r * bonus * pp

applyDamage :: GameWorld -> Unit -> Float -> (GameWorld, [Unit])
applyDamage gw u dmg = if dmg >= pp u
                         then removeUnit gw [u]
                         else updateUnit gw (u { pp = pp u - dmg }) []

getDamageBonus :: GameWorld -> Unit -> Unit -> IO (Float, Float)
getDamageBonus gw att def = do
    r1 <- randomRIO (0, 0.3)
    r2 <- randomRIO (0, 0.3)
    return (sum [r1, attTileBonus, attAdjUnitsBonus, attTraitBonus], sum [r1, defTileBonus, defAdjUnitsBonus, defTraitBonus])
  where
    attTileBonus = tileAttBonus $ getUnitTile gw att
    defTileBonus = tileDefBonus $ getUnitTile gw def

    attAdjUnitsBonus = sum [if team u == team att then 0.1 else -0.1 | u <- getAdjUnits gw att]
    defAdjUnitsBonus = sum [if team u == team def then 0.1 else -0.1 | u <- getAdjUnits gw def]

    attTraitBonus = sum [applyCombatTrait t def | t <- getCombatTraits att]
    defTraitBonus = sum [applyCombatTrait t att | t <- getCombatTraits def]

    getCombatTraits u = filter (isJust . traitPp) $ traits u
