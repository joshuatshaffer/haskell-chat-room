
module ChatServer (chatAsServer) where

import Common

import System.IO
import Control.Concurrent
import Control.Monad
import Network
import Data.List (delete)

serviceClient :: Handle -> MVar (Handle, String) -> MVar [Handle] -> IO ()
serviceClient h messageBuf clients = do
  modifyMVar_ clients (\xs -> return (h:xs))
  _ <- forkFinally (receiveMessages h messageBuf)
                   (\_ -> modifyMVar_ clients (return . delete h))
  return ()

relayMessages :: MVar (Handle, String) -> MVar [Handle] -> IO ()
relayMessages messageBuf clients = forever $ do
  (sender,message) <- takeMVar messageBuf
  putStrLn $ "relaying " ++ show (sender,message)
  withMVar clients $ mapM_ (\h -> when (h /= sender) $ hPutStrLn h message)

chatAsServer :: PortID -> IO ()
chatAsServer port = do
  clients <- newMVar []
  messageBuf <- newEmptyMVar
  _ <- forkIO (relayMessages messageBuf clients)
  s <- listenOn port
  _ <- forever $ do
    (h,hn,p) <- accept s
    print (hn,p)
    serviceClient h messageBuf clients
  sClose s
