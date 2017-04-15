
module ChatClient (chatAsClient) where

import Common

import System.IO
import Control.Concurrent
import Control.Monad
import Network

data ClientEvent = Join String
                 | Leave String
                 | Broadcast String String
                 | ChangeName String String
                 | DirectMessage String String
                 | WeBroadcast String
                 | WeChangeName String
                 | WeDirectMessage String String

messageToEvent :: Server2ClientMessage -> ClientEvent
messageToEvent (S2C_J who)               = Join who
messageToEvent (S2C_L who)               = Leave who
messageToEvent (S2C_BC fromWhom message) = Broadcast fromWhom message
messageToEvent (S2C_CN who newName)      = ChangeName who newName
messageToEvent (S2C_DM fromWhom message) = DirectMessage fromWhom message

messageToEvent' :: Server2ClientMessage -> MVar ClientEvent -> IO ()
messageToEvent' m nextEvent = putMVar nextEvent $ messageToEvent m

receiveMessages :: Handle -> MVar ClientEvent -> IO ()
receiveMessages h nextEvent = forever $ do
  m <- fmap read (hGetLine h)
  messageToEvent' m nextEvent

handleEvents :: MVar ClientEvent -> Handle -> IO ()
handleEvents nextEvent h = forever $ takeMVar nextEvent >>= handleEvent h

handleEvent :: Handle -> ClientEvent -> IO ()
handleEvent _ (Join who) = putStrLn $ who ++ " joined the room."
handleEvent _ (Leave who) = putStrLn $ who ++ " left the room."
handleEvent _ (Broadcast fromWhom message) = putStrLn $ fromWhom ++ "> " ++ message
handleEvent _ (ChangeName who newName) = putStrLn $ who ++ " changed their name to " ++ newName
handleEvent _ (DirectMessage fromWhom message) = putStrLn $ fromWhom ++ " says to you> " ++ message

handleEvent h (WeBroadcast message) = hPrint h $ C2S_BC message
handleEvent h (WeChangeName newName) = hPrint h $ C2S_CN newName
handleEvent h (WeDirectMessage toWhom message) = hPrint h $ C2S_DM toWhom message

handleUserInput :: MVar ClientEvent -> IO ()
handleUserInput nextEvent = forever $ getLine >>= (putMVar nextEvent . WeBroadcast)

chatAsClient :: HostName -> PortID -> IO ()
chatAsClient name port = do
  nextEvent <- newEmptyMVar
  h <- connectTo name port
  together [handleEvents nextEvent h
           ,receiveMessages h nextEvent
           ,handleUserInput nextEvent
           ]
