{-# LANGUAGE RecordWildCards #-}   
import Data.Binary
import qualified Data.ByteString as BS 

data Unitit = Unitit { unitit :: [Unit]} deriving (Show)
instance Binary Unitit where
    put Unitit{..} = do
        put unitit
    get = do
        unitit <- get
        return Unitit{..}

data Unit = Unit { name :: String
                 , ap :: Int
                 , pp :: Float
                 , traits :: [Trait]
                 , team :: Int
                 , animFrame :: Int
            } deriving (Show)	    
instance Binary Unit where
    put Unit{..} = do
        put name
        put ap
        put pp
        put traits
        put team
        put animFrame
    get = do
        name <- get
        ap <- get
        pp <- get
        traits <- get
        team <- get
        animFrame <- get
        return Unit{..}

data Trait = Trait { traitAp :: Int
                   , traitPp :: Int
                   , duration :: Int } deriving (Show)

instance Binary Trait where 
    put Trait{..} = do
        put traitAp
        put traitPp
        put duration
    get = do
        traitAp <- get
        traitPp <- get
        duration <- get
        return Trait{..}

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
    
    let traitti1 = Trait{traitAp = 10, traitPp = 10, duration = 5}
    let traitti2 = Trait{traitAp = 5, traitPp = 12, duration = 6}
    let unitti1 = Unit{name ="Pekka", ap=5, pp=9,traits =[traitti1,traitti2], team = 1,animFrame=0}
    let unitti2 = Unit{name ="Matti", ap=5, pp=9,traits =[traitti1,traitti2], team = 1,animFrame=0}
    let unitteja = Unitit{unitit = [unitti1, unitti2]}
    let unittejenEnkoodaus = encode unitteja
    print unittejenEnkoodaus
   -- let pituus = length unittejenEnkoodaus
   -- print pituus
    let unittejenDekoodaus = decode unittejenEnkoodaus
    print (unittejenDekoodaus :: Unitit)

