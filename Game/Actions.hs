module Game.Actions where

import Game.GameWorld (GameWorld, updateUnit, removeUnit, getUnitTile, getAdjUnits, getUnitAt)
import Game.Unit (Unit(..), Trait(..), moveUnit, applyCombatTrait)
import Game.Position (Position)
import Game.Tile (Tile, tileAttBonus, tileDefBonus)
import System.Random (randomRIO)
import Data.Maybe (isNothing)

action :: GameWorld -> Unit -> Position -> IO (GameWorld, [Unit])
action gw u pos = case getUnitAt gw pos of
                    Nothing -> return (move gw u pos, [])
                    Just target -> if team target == team u then return (gw, []) else smack gw u target

-- | Siirtää yksikön annettuun paikkaan.
-- Huom! Oikeellisuustarkistukset tehdään aiemmin
-- (ei pitäis olla päällekkäisyyksiä)
move :: GameWorld -> Unit -> Position -> GameWorld
move gw u pos = updateUnit gw (moveUnit u pos)

-- | Huitasee ukkelia - palauttaa pelimaailman, sekä listan kuolleista yksiköistä.
smack :: GameWorld -> Unit -> Unit -> IO (GameWorld, [Unit])
smack gw att def = do
    (attBonus, defBonus) <- getDamageBonus gw att def
    attDmg <- dmg attBonus (pp att)
    defDmg <- dmg defBonus (pp def)
    putStrLn $ "attDmg: " ++ show attDmg ++ " defDmg: " ++ show defDmg
    let (gw', dead) = applyDamage gw att defDmg
    let (gw'', dead') = applyDamage gw' def attDmg
    return (gw'', dead ++ dead')
  where dmg bonus pp = randomRIO (0.5,1.0) >>= \r -> return (r * bonus * pp)

applyDamage :: GameWorld -> Unit -> Float -> (GameWorld, [Unit])
applyDamage gw u dmg = if dmg >= pp u
                         then (removeUnit gw u, [u])
                         else (updateUnit gw (u { pp = pp u - dmg }), [])

getDamageBonus :: GameWorld -> Unit -> Unit -> IO (Float, Float)
getDamageBonus gw att def = do
    let (r1,r2) = (0.3, 0.3) -- todo: ota randomit
    return (sum [r1, attTileBonus, attAdjUnitsBonus, attTraitBonus], sum [r2, defTileBonus, defAdjUnitsBonus, defTraitBonus])
  where
    attTileBonus = tileAttBonus $ getUnitTile gw att
    defTileBonus = tileDefBonus $ getUnitTile gw def

    attAdjUnitsBonus = sum [if team u == team att then 0.1 else -0.1 | u <- getAdjUnits gw att]
    defAdjUnitsBonus = sum [if team u == team def then 0.1 else -0.1 | u <- getAdjUnits gw def]

    attTraitBonus = sum [applyCombatTrait t def | t <- getCombatTraits att]
    defTraitBonus = sum [applyCombatTrait t att | t <- getCombatTraits def]

    getCombatTraits u = filter (isNothing . traitAp) $ traits u
