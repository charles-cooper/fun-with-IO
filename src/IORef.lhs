> module Main where

> import Data.IORef

> import Control.Concurrent

This is an example using IORef.

> ioref :: Int -> String -> IO ()
> ioref delay str = do
>  ref <- newIORef str

`forkIO` will spawn a new (runtime) thread.

>  forkIO $ do
>    writeIORef ref (reverse str)

Let's delay `n` microseconds. Depending on the scheduler we should get different results.

>  threadDelay delay
>  readIORef ref >>= putStrLn

> main :: IO ()
> main = do

Let's run the first example!

>  ioref 1 "Hello, world!"
>  ioref 0 "Hello, world!"

