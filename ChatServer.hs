
module ChatServer (chatAsServer) where

import           Common

import           Control.Concurrent
import           Control.Monad
import           Data.Maybe         (fromJust)
import           Network
import           System.IO


data ServerEvent = Join Handle String
                 | Leave Handle
                 | Broadcast Handle String
                 | ChangeName Handle String
                 | DirectMessage Handle String String
                 deriving (Show)

messageToEvent :: Client2ServerMessage -> Handle -> ServerEvent
messageToEvent (C2S_BC message) h        = Broadcast h message
messageToEvent (C2S_CN newName) h        = ChangeName h newName
messageToEvent (C2S_DM toWhom message) h = DirectMessage h toWhom message

messageToEvent' :: Client2ServerMessage -> Handle -> MVar ServerEvent -> IO ()
messageToEvent' m h nextEvent = putMVar nextEvent $! messageToEvent m h

serviceClient :: Handle -> MVar ServerEvent -> IO ()
serviceClient h nextEvent =
  void $ forkFinally (onFirstMessage >> forever onRestMessages) (\_ -> putMVar nextEvent $ Leave h)
  where
    onFirstMessage = do m <- fmap read (hGetLine h)
                        case m of
                          C2S_CN newName -> putMVar nextEvent $ Join h newName
                          _ -> do putMVar nextEvent $! Join h ""
                                  messageToEvent' m h nextEvent

    onRestMessages = do m <- fmap read (hGetLine h)
                        messageToEvent' m h nextEvent

tellAllExcept :: [(String, Handle)] -> Handle -> Server2ClientMessage -> IO ()
tellAllExcept hs x m = mapM_ (\(_,h) -> when (h /= x) $ hPrint h m) hs

handleEvents :: MVar ServerEvent -> IO ()
handleEvents nextEvent = handleEvents' []
  where handleEvents' clients = do e <- takeMVar nextEvent
                                   newClients <- handleEvent e clients
                                   handleEvents' newClients

handleEvent :: ServerEvent -> [(String, Handle)] -> IO [(String, Handle)]

handleEvent (Join h newName) clients =
  do tellAllExcept clients h $ S2C_J newName
     return $ (newName,h):clients

handleEvent (Leave h) clients =
  do tellAllExcept clients h . S2C_L . fromJust $ unlookup h clients
     return $ filter ((/= h) . snd) clients

handleEvent (Broadcast h message) clients =
  do tellAllExcept clients h $ S2C_BC (fromJust $ unlookup h clients) message
     return clients

handleEvent (ChangeName h newName) clients =
  do tellAllExcept clients h $ S2C_CN (fromJust $ unlookup h clients) newName
     return $ (newName,h) : filter ((/= h) . snd) clients

handleEvent (DirectMessage h toWhom message) clients =
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
