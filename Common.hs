
module Common where

import System.IO
import Control.Concurrent
import Control.Monad

receiveMessages :: Handle -> MVar (Handle, String) -> IO ()
receiveMessages h messageBuf = forever $ do
  m <- hGetLine h
  putMVar messageBuf (h,m)

together :: [IO ()] -> IO ()
together ios = do
  onDone <- newEmptyMVar
  ids <- mapM (\x -> forkFinally x (\_ -> putMVar onDone ())) ios
  takeMVar onDone
  mapM_ killThread ids
