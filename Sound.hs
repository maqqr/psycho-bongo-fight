{-# LANGUAGE ForeignFunctionInterface #-}
module Sound where

--import Foreign
import Foreign.C
import Control.Monad
import Control.Applicative

data SoundConfig = SoundConfig {
    path :: String,
    soundDevice :: Int,
    frequency :: Int
} deriving (Eq, Show)

newtype Channel = Channel CUInt deriving (Eq, Show)

defaultSoundConfig :: SoundConfig
defaultSoundConfig = SoundConfig "" (-1) 44100

withSound :: SoundConfig -> IO () -> IO ()
--withSound (SoundConfig path' device freq) doAction = withCString path' $ \cstr -> do
--    err <- c'initialize cstr (fromIntegral device) (fromIntegral freq)
withSound (SoundConfig _ device freq) doAction = do
    err <- c'initialize (fromIntegral device) (fromIntegral freq)
    when (err == 0) doAction
    c'cleanup

class Playable a where
    filename :: a -> String

playStream :: Playable a => a -> Float -> Bool -> IO Channel
playStream music volume loop = Channel <$> withCString (filename music) (\s -> c'playStream s volume (boolToInt loop))
    where
        boolToInt :: Bool -> CInt
        boolToInt True  = 1
        boolToInt False = 0


--playSample :: Playable a => a -> IO Channel
--playSample sample = Channel <$> withCString (filename sample) c'playSample

foreign import ccall unsafe "sound.h initialize" c'initialize
    :: CInt -> CInt -> IO CInt

foreign import ccall unsafe "sound.h playStream" c'playStream
    :: CString -> Float -> CInt -> IO CUInt

--foreign import ccall unsafe "asdf.h playSample" c'playSample
--    :: CString -> IO CUInt

foreign import ccall unsafe "sound.h channelStop" c'channelStop
    :: CUInt -> IO CInt

foreign import ccall unsafe "sound.h cleanup" c'cleanup
    :: IO ()
