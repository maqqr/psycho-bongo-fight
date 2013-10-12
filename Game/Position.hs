module Game.Position where

import qualified Game.Resources as R

type Position = (Int, Int)

-- | Lyhenne fromIntegral funktiolle
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

-- | Muuntaa pelikoordinaatit isometrisiksi ruutukoordinaateiksi
toIsom :: Position -> (Float, Float)
toIsom (x, y) = ((fi y * hw) + (fi x * hw), (fi x * hh) - (fi y * hh))
    where
        hw = R.tileWidth / 2.0
        hh = R.tileHeight / 2.0

-- | Muuntaa ruutukoordinaatit pelikoordinaateiksi
fromIsom :: (Float, Float) -> Position
fromIsom (x, y) = (floor ((x + 2*y) / R.tileWidth), floor (-(2*y - x) / R.tileWidth))

-- | Muuntaa hiiren sijainnin pelikoordinaatiksi
convertMouse :: (Float, Float) -> (Float, Float) -> Position
convertMouse (sx, sy) (x, y) = let (x', y') = fromIsom (x - sx, y + 64 - sy) in (y' + 1, x')
