module Main where

import Data.IORef

import Control.Concurrent

ioref :: String -> IO ()
ioref str = do
  ref <- newIORef str
  forkIO $ do
    hello <- readIORef ref
    writeIORef ref $ reverse str
  -- what will this print!
  readIORef ref >>= putStrLn
  readIORef ref >>= putStrLn

main :: IO ()
main = do
  ioref "Hello, world!"

