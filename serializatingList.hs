{-# LANGUAGE RecordWildCards #-}   
import Data.Binary

data Lista = Lista { lukuja :: [[Int]] } deriving (Show)
instance Binary Lista where
	put Lista{..} = do
		put lukuja
	get = do
		lukuja <- get
		return Lista{..}

main :: IO ()
main = do
	let lista = Lista {lukuja = [[4,6,9], [54,2,234,23452,2535,235,235,3332,1246]]}
	print lista
	let e = encode lista
	print e
	let d = decode e
	print (d :: Lista)