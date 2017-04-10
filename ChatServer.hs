
import Network
import ChatCommon

main :: IO ()
main = do
  s <- listenOn (PortNumber 4242)
  (h,_,_) <- accept s
  chatWith h
  sClose s
