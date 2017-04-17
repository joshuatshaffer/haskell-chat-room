
module ChatClient (chatAsClient) where

import Common

import System.IO
import Control.Concurrent
import Control.Monad
import Network

type ClientEvent = Handle -> IO ()

messageToEvent :: Server2ClientMessage -> ClientEvent
messageToEvent (S2C_J who)               = joinEvent who
messageToEvent (S2C_L who)               = leaveEvent who
messageToEvent (S2C_BC fromWhom message) = broadcastEvent fromWhom message
messageToEvent (S2C_CN who newName)      = changeNameEvent who newName
messageToEvent (S2C_DM fromWhom message) = directMessageEvent fromWhom message

messageToEvent' :: Server2ClientMessage -> MVar ClientEvent -> IO ()
messageToEvent' m nextEvent = putMVar nextEvent $ messageToEvent m

receiveMessages :: Handle -> MVar ClientEvent -> IO ()
receiveMessages h nextEvent = forever $ do
  m <- fmap read (hGetLine h)
  messageToEvent' m nextEvent

handleEvents :: MVar ClientEvent -> Handle -> IO ()
handleEvents nextEvent h = forever $ do e <- takeMVar nextEvent
                                        e h
joinEvent :: String -> ClientEvent
joinEvent who _ = putStrLn $ who ++ " joined the room."

leaveEvent :: String -> ClientEvent
leaveEvent who _ = putStrLn $ who ++ " left the room."

broadcastEvent :: String -> String -> ClientEvent
broadcastEvent fromWhom message _ = putStrLn $ fromWhom ++ "> " ++ message

changeNameEvent :: String -> String -> ClientEvent
changeNameEvent who newName _ = putStrLn $ who ++ " changed their name to " ++ newName

directMessageEvent :: String -> String -> ClientEvent
directMessageEvent fromWhom message _ = putStrLn $ fromWhom ++ " says to you> " ++ message

weBroadcastEvent :: String -> ClientEvent
weBroadcastEvent message h = hPrint h $ C2S_BC message

weChangeNameEvent :: String -> ClientEvent
weChangeNameEvent newName h = hPrint h $ C2S_CN newName

weDirectMessageEvent :: String -> String -> ClientEvent
weDirectMessageEvent toWhom message h = hPrint h $ C2S_DM toWhom message

handleUserInput :: MVar ClientEvent -> IO ()
handleUserInput nextEvent = forever $ do s <- getLine
                                         case head s of
                                           ':' -> putMVar nextEvent $ weChangeNameEvent $ tail s
                                           ';' -> do s1 <- getLine
                                                     putMVar nextEvent $ weDirectMessageEvent (tail s) s1
                                           _ -> putMVar nextEvent $ weBroadcastEvent s

chatAsClient :: HostName -> PortID -> IO ()
chatAsClient name port = do
  nextEvent <- newEmptyMVar
  h <- connectTo name port
  together [handleEvents nextEvent h
           ,receiveMessages h nextEvent
           ,handleUserInput nextEvent
           ]
