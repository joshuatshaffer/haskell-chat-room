
import Network
import ChatCommon

main :: IO ()
main = do
  h <- connectTo "localhost" (PortNumber 4242)
  chatWith h
