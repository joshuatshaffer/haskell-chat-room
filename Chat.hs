
import System.IO
import System.Environment
import Control.Concurrent
import Network

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


chatAsClient :: HostName -> PortID -> IO ()
chatAsClient name port = do
  h <- connectTo name port
  chatWith h

chatAsServer :: PortID -> IO ()
chatAsServer port = do
  s <- listenOn port
  (h,_,_) <- accept s
  chatWith h
  sClose s

main :: IO ()
main = do
  args <- getArgs
  case length args of
    2 -> chatAsClient (head args) (PortNumber . read $ args !! 1)
    1 -> chatAsServer (PortNumber . read $ head args)
    _ -> undefined
