> {-# LANGUAGE QuasiQuotes #-}
> module Main where

> import           Control.Concurrent.STM
> import           Control.Concurrent (forkIO, threadDelay)
> import           Control.Monad      (forever, forM_)

> import qualified Data.Map as Map
> import           Data.Map (Map(..), (!))
> import           Data.Monoid ((<>))

> import           Text.InterpolatedString.Perl6 (qc)

This is an example using STM.

Let's run a transaction. First define a 'bank', which is a map from `String`s (account names) to balances.

> type Bank = Map String (TVar Int)
>
> balance :: Bank -> IO Int
> balance bank = atomically $ do
>   accounts <- mapM readTVar bank
>   return $ sum $ Map.elems accounts
>
> transfer :: String -> String -> Int -> Bank -> IO ()
> transfer recipient src qty bank = atomically $ do
>   modifyTVar (bank ! src)       (subtract qty)
>   modifyTVar (bank ! recipient) (add qty)
>  where add = flip (+)

> unbusted_transaction :: IO ()
> unbusted_transaction = do

Create a 'bank', which is a map from names to 'account balances' which are hidden inside MVars

>   bank <- atomically $ mapM newTVar $ Map.fromList $
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

Let's run the second example!

>  unbusted_transaction

