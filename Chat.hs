
import ChatClient
import ChatServer

import System.Environment
import Network

main :: IO ()
main = do
  args <- getArgs
  case length args of
    2 -> chatAsClient (head args) (PortNumber . read $ args !! 1)
    1 -> chatAsServer (PortNumber . read $ head args)
    _ -> undefined
