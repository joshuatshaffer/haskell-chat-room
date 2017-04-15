
module Common where

import Control.Concurrent

data Client2ServerMessage = C2S_BC String
                          | C2S_CN String
                          | C2S_DM String String
                          deriving (Show,Read)

data Server2ClientMessage = S2C_J  String
                          | S2C_L  String
                          | S2C_BC String String
                          | S2C_CN String String
                          | S2C_DM String String
                          deriving (Show,Read)

together :: [IO ()] -> IO ()
together ios = do
  onDone <- newEmptyMVar
  ids <- mapM (\x -> forkFinally x (\_ -> putMVar onDone ())) ios
  takeMVar onDone
  mapM_ killThread ids

unlookup :: Eq b => b -> [(a,b)] -> Maybe a
unlookup k xs = lookup k $ map (\(a,b)->(b,a)) xs
