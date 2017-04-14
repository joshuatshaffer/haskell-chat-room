
module ChatServer (chatAsServer) where

import Common

import System.IO
import Control.Concurrent
import Control.Monad
import Network
import Data.List (delete)

newConnection :: Handle -> MVar [Handle] -> IO ()
newConnection h clients = do
  putStrLn $ show h ++ " joined the room."
  modifyMVar_ clients (\xs -> return (h:xs))

endConnection :: Handle -> MVar [Handle] -> IO ()
endConnection h clients = do
  putStrLn $ show h ++ " left the room."
  modifyMVar_ clients (return . delete h)

serviceClient :: Handle -> MVar (Handle, String) -> MVar [Handle] -> IO ()
serviceClient h messageBuf clients = do
  newConnection h clients
  _ <- forkFinally (receiveMessages h messageBuf)
                   (\_ -> endConnection h clients)
  return ()

relayMessages :: MVar (Handle, String) -> MVar [Handle] -> IO ()
relayMessages messageBuf clients = forever $ do
  (sender,message) <- takeMVar messageBuf
  putStrLn $ show sender ++ " said " ++ show message
  withMVar clients $ mapM_ (\h -> when (h /= sender) $ hPutStrLn h message)

chatAsServer :: PortID -> IO ()
chatAsServer port = do
  clients <- newMVar []
  messageBuf <- newEmptyMVar
  _ <- forkIO (relayMessages messageBuf clients)
  s <- listenOn port
  _ <- forever $ do
    (h,_,_) <- accept s
    serviceClient h messageBuf clients
  sClose s
