module Main where
import Data.UUID
import Data.UUID.V4
import Network.Simple.TCP
import Control.Concurrent.MVar
import qualified Data.ByteString.Char8 as B

blockSize :: Int
blockSize = 1024 * 4

clientHandler :: (UUID, Socket) -> MVar [(UUID, Socket)] -> IO ()
clientHandler (uuid, sock) clients = do
    putStrLn "receiving data..."
    maybePacket <- recv sock blockSize
    case maybePacket of
        Just packet -> do
            putStrLn "data broadcast"
            broadcast packet
            clientHandler (uuid, sock) clients
        Nothing -> putStrLn "Client closed connection."
    where
        broadcast :: B.ByteString -> IO ()
        broadcast dataa = do
            cli <- takeMVar clients
            mapM_ (\(_, sock') -> send sock' dataa) cli
            putMVar clients cli

main :: IO ()
main = withSocketsDo $ do
    first <- newEmptyMVar :: IO (MVar Socket)
    clients <- newEmptyMVar :: IO (MVar [(UUID, Socket)])
    putMVar clients []
    putStrLn "Starting server..."
    serve (Host "127.0.0.1") "44444" $ \(sock, remoteAddr) -> do
        putStrLn $ "TCP connection from " ++ show remoteAddr
        uuid <- nextRandom
        first' <- isEmptyMVar first
        if first' then do
            putStrLn "Eka pelaaja yhdisti"
            putMVar first sock
            else do
                putStrLn "Toka pelaaja yhdisti. Peli alkaa."
                firstsock <- takeMVar first
                send firstsock (B.pack "0")
                send sock (B.pack "1")
                putMVar first firstsock
        cli <- takeMVar clients
        putMVar clients $ (uuid, sock) : cli
        clientHandler (uuid, sock) clients
