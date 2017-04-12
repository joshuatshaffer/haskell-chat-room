
import System.IO
import System.Environment
import Control.Concurrent
import Control.Monad
import Network
import Data.List (delete)

type ConectionPoint = MVar String

handleTx :: Handle -> ConectionPoint -> IO ()
handleTx h cp = forever (takeMVar cp >>= hPutStrLn h)

handleRx :: Handle -> ConectionPoint -> MVar [ConectionPoint] -> IO ()
handleRx h cp cps = forever (hGetLine h >>= (\m -> withMVar cps (mapM_ (\cp' -> when (cp' /= cp) $ putMVar cp' m))))

startConnection :: Handle -> Handle -> MVar [ConectionPoint] -> IO (ConectionPoint, MVar (), ThreadId, ThreadId)
startConnection tx rx cps = do
  cp <- newEmptyMVar
  modifyMVar_ cps (\cps'-> return $ cp:cps')
  onDone <- newEmptyMVar
  txtid <- forkFinally (handleTx tx cp) (\_ -> putMVar onDone ())
  rxtid <- forkFinally (handleRx rx cp cps) (\_ -> putMVar onDone ())
  return (cp,onDone,txtid,rxtid)

endConnection :: (ConectionPoint, MVar (), ThreadId, ThreadId) -> MVar [ConectionPoint] -> IO ()
endConnection (cp,onDone,txtid,rxtid) cps = do
  takeMVar onDone
  killThread txtid
  killThread rxtid
  modifyMVar_ cps (return . delete cp)

makeConnection :: Handle -> MVar [ConectionPoint] -> IO ()
makeConnection h cps = do
  con <- startConnection h h cps
  _ <- forkIO $ endConnection con cps
  return ()

makeTerminalConnection :: MVar [ConectionPoint] -> IO ()
makeTerminalConnection cps = do
  con <- startConnection stdout stdin cps
  _ <- forkIO $ endConnection con cps
  return ()

chatAsClient :: HostName -> PortID -> IO ()
chatAsClient name port = do
  cps <- newEmptyMVar
  putMVar cps []

  con <- startConnection stdout stdin cps

  h <- connectTo name port
  makeConnection h cps

  endConnection con cps


chatAsServer :: PortID -> IO ()
chatAsServer port = do
  cps <- newEmptyMVar
  putMVar cps []
  makeTerminalConnection cps
  s <- listenOn port
  forever $ do (h,hn,p) <- accept s
               print (hn,p)
               makeConnection h cps
  sClose s

main :: IO ()
main = do
  args <- getArgs
  case length args of
    2 -> chatAsClient (head args) (PortNumber . read $ args !! 1)
    1 -> chatAsServer (PortNumber . read $ head args)
    _ -> undefined
