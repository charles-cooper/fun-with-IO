> {-# LANGUAGE QuasiQuotes #-}
> module Main where

> import           Control.Concurrent.MVar
> import           Control.Concurrent (forkIO, threadDelay)
> import           Control.Monad      (forever, forM_)

> import qualified Data.Map as Map
> import           Data.Map (Map(..), (!))
> import           Data.Monoid ((<>))

> import           Text.InterpolatedString.Perl6 (qc)

This is an example using MVar. MVars are Haskell's answer to locks, and they behave similarly. An MVar can be thought of as a box, which can be empty or full. 'takeMVar' is like acquiring a lock, 'putMVar' is like releasing a lock.

> mvar :: Int -> String -> IO ()
> mvar delay str = do
>  ref <- newMVar str

`forkIO` will spawn a new (runtime) thread.

>  forkIO $ do
>    putMVar ref (reverse str)

Let's delay `n` microseconds. Now that we are using MVar, the forked thread cannot put anything into the mvar until it has been emptied. This function will always print the same thing no matter the delay.

>  threadDelay delay
>  takeMVar ref >>= putStrLn

Let's run a transaction. First define a 'bank', which is a map from `String`s (account names) to balances.

> type Bank = Map String (MVar Int)
>
> balance :: Bank -> IO Int
> balance bank = do

To calculate the balance, we lock every single account in the bank, run a sum function to calculate the bank's overall balance, then unlock every account in the bank.

>   accounts <- mapM takeMVar bank
>   let ret = sum $ Map.elems accounts
>   mapM (uncurry putMVar) $ Map.intersectionWith (,) bank accounts
>   return ret
>
> transfer :: String -> String -> Int -> Bank -> IO ()
> transfer recipient src qty bank = do

This time, let's wrap the transaction in a pair of locks. That way when another thread tries to access the accounts to read their memory, they should always see a consistent view.

.. Unfortunately this code is not correct either, it will result in a deadlock. Can you spot the mistake?

>   modifyMVar_ (bank ! src) $ \srcBalance -> do
>     threadDelay 1
>     modifyMVar_ (bank ! recipient) $ \dstBalance -> do
>       return $ dstBalance + qty
>     return $ srcBalance - qty

> busted_transaction :: IO ()
> busted_transaction = do

Create a 'bank', which is a map from names to 'account balances' which are hidden inside MVars

>   bank <- mapM newMVar $ Map.fromList $
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

>  mvar 1 "Hello, world!"
>  mvar 0 "Hello, world!"

Let's run the second example!

>  busted_transaction

