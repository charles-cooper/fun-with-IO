> {-# LANGUAGE QuasiQuotes #-}
> module Main where

> import           Data.IORef
> import           Control.Concurrent (forkIO, threadDelay)
> import           Control.Monad      (forever, forM_)

> import qualified Data.Map as Map
> import           Data.Map (Map(..), (!))
> import           Data.Monoid ((<>))

> import           Text.InterpolatedString.Perl6 (qc)

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

Let's run a transaction. First define a 'bank', which is a map from `String`s (account names) to balances.

> type Bank = Map String (IORef Int)
>
> balance :: Bank -> IO Int
> balance bank = do
>   balances <- Map.elems <$> mapM readIORef bank
>   return $ sum balances
>
> transfer :: String -> String -> Int -> Bank -> IO ()
> transfer recipient src qty bank = do
>   modifyIORef (bank ! src)       (subtract qty)
>   balance1 <- balance bank
>   putStrLn $ "In the middle of a transaction .. balance = "
>            <> (show balance1)
>   threadDelay 1
>   modifyIORef (bank ! recipient) (add qty)
>
>   where
>   add = flip (+)

> busted_transaction :: IO ()
> busted_transaction = do

Create a 'bank', which is a map from names to 'account balances' which are hidden inside IORefs.

>   bank <- mapM newIORef $ Map.fromList $
>     [ ("Alice", 50)
>     , ("Zoe"  , 50)
>     ]

Start a worker thread to poll the bank accounts and see if the balance ever drops below 50

>   forkIO $ forever $ do
>     balance bank >>= \b -> if b < 100
>       then do
>         putStrLn "Horrible day, bank balance under 100!"
>         error "Alert the prime minister ('uncatchable' exception)"
>       else return ()

>   balance1 <- balance bank
>   putStrLn [qc|Transaction starting .. beginning balance = {balance1}|]
>   transfer "Alice" "Zoe" 25 bank
>   balance2 <- balance bank
>   putStrLn [qc|Transaction complete .. ending balance = {balance2}|]

> main :: IO ()
> main = do

Let's run the first example!

>  ioref 1 "Hello, world!"
>  ioref 0 "Hello, world!"

Let's run the second example!

>  busted_transaction

