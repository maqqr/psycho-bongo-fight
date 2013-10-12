{-# LANGUAGE RecordWildCards #-}   
import Data.Binary

data Unit = Unit { name :: String
                 , ap :: Int
                 , pp :: Float
                 , traits :: [Trait]
                 , team :: Int
                 , animFrame :: Int
            } deriving (Show)	    

data Trait = Trait { traitAp :: Int
                   , traitPp :: Int
                   , duration :: Int } deriving (Show)

data Testi = Testi {nimi :: String, toinenNimi :: String, koko :: Integer} deriving (Show)
instance Binary Testi where
    put Testi{..} = do
    	put nimi
	put toinenNimi
	put koko
    get = do
    	nimi <- get
	toinenNimi <- return "Default"
	koko <- get
	return Testi{..}

main :: IO ()
main = do
    let testi = Testi {nimi="Testi jou jou", toinenNimi = "Jep", koko=500+2^100*5}
    let enkoodaus = encode testi
    print enkoodaus
    let dekoodaus = decode enkoodaus
    print (dekoodaus:: Testi)

