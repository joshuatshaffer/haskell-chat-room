
module ChatClient (chatAsClient) where

import Common

import System.IO
import Control.Concurrent
import Control.Monad
import Network

handleMessages :: MVar (Handle, String) -> Handle -> IO ()
handleMessages messageBuf h = forever $ do
  (sender,message) <- takeMVar messageBuf
  if sender == stdin
    then hPutStrLn h message
    else putStrLn message

chatAsClient :: HostName -> PortID -> IO ()
chatAsClient name port = do
  messageBuf <- newEmptyMVar
  h <- connectTo name port
  together [handleMessages messageBuf h
           ,receiveMessages h messageBuf
           ,receiveMessages stdin messageBuf
           ]
