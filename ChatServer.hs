
module ChatServer (chatAsServer) where

import           Common

import           Control.Concurrent
import           Control.Monad
import           Data.Maybe         (fromJust)
import           Network
import           System.IO


type ServerEvent = [(String, Handle)] -> IO [(String, Handle)]

messageToEvent :: Client2ServerMessage -> Handle -> ServerEvent
messageToEvent (C2S_BC message) h        = broadcastEvent h message
messageToEvent (C2S_CN newName) h        = changeNameEvent h newName
messageToEvent (C2S_DM toWhom message) h = directMessageEvent h toWhom message

messageToEvent' :: Client2ServerMessage -> Handle -> MVar ServerEvent -> IO ()
messageToEvent' m h nextEvent = putMVar nextEvent $! messageToEvent m h

serviceClient :: Handle -> MVar ServerEvent -> IO ()
serviceClient h nextEvent =
  void $ forkFinally (onFirstMessage >> forever onRestMessages) (\_ -> putMVar nextEvent $ leaveEvent h)
  where
    onFirstMessage = do m <- fmap read (hGetLine h)
                        case m of
                          C2S_CN newName -> putMVar nextEvent $ joinEvent h newName
                          _ -> do putMVar nextEvent $! joinEvent h ""
                                  messageToEvent' m h nextEvent

    onRestMessages = do m <- fmap read (hGetLine h)
                        messageToEvent' m h nextEvent

tellAllExcept :: [(String, Handle)] -> Handle -> Server2ClientMessage -> IO ()
tellAllExcept hs x m = mapM_ (\(_,h) -> when (h /= x) $ hPrint h m) hs

handleEvents :: MVar ServerEvent -> IO ()
handleEvents nextEvent = handleEvents' []
  where handleEvents' clients = do e <- takeMVar nextEvent
                                   newClients <- e clients
                                   handleEvents' newClients

joinEvent :: Handle -> String -> ServerEvent
joinEvent h newName clients =
  do tellAllExcept clients h $ S2C_J newName
     return $ (newName,h):clients

leaveEvent :: Handle -> ServerEvent
leaveEvent h clients =
  do tellAllExcept clients h . S2C_L . fromJust $ unlookup h clients
     return $ filter ((/= h) . snd) clients

broadcastEvent :: Handle -> String -> ServerEvent
broadcastEvent h message clients =
  do tellAllExcept clients h $ S2C_BC (fromJust $ unlookup h clients) message
     return clients

changeNameEvent :: Handle -> String -> ServerEvent
changeNameEvent h newName clients =
  do tellAllExcept clients h $ S2C_CN (fromJust $ unlookup h clients) newName
     return $ (newName,h) : filter ((/= h) . snd) clients

directMessageEvent :: Handle -> String -> String -> ServerEvent
directMessageEvent h toWhom message clients =
  do hPrint (fromJust $ lookup toWhom clients) $ S2C_DM (fromJust $ unlookup h clients) message
     return clients

chatAsServer :: PortID -> IO ()
chatAsServer port = do
  nextEvent <- newEmptyMVar
  _ <- forkIO (handleEvents nextEvent)
  s <- listenOn port
  _ <- forever $ do
    (h,_,_) <- accept s
    serviceClient h nextEvent
  sClose s
