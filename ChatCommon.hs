
module ChatCommon where

import System.IO
import Control.Concurrent

pipeHandles :: Handle -> Handle -> IO ()
pipeHandles from to = hGetContents from >>= hPutStr to

chatWith :: Handle -> IO ()
chatWith h = do
  hSetBuffering stdin NoBuffering
  done <- newEmptyMVar
  rxId <- forkFinally (pipeHandles h stdout) (\_ -> putMVar done ())
  txId <- forkFinally (pipeHandles stdin h) (\_ -> putMVar done ())
  takeMVar done
  killThread txId
  killThread rxId
  hSetBuffering stdin LineBuffering
