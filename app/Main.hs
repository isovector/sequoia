{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.FRPNow
import Control.Applicative

n = 11

main = runNowMaster (test n)

test :: Int -> Now (Event ())
test n = do b <- count
            a <- sample b
            sync . putStrLn $ show a
            e <- sample (when ((n ==) <$> b))
            return e

magic :: (Int -> a) -> Now (Behavior a)
magic f = loop 0 where
  loop i =  do  e <- async (return ())
                e'<- planNow (loop (i+1) <$ e)
                return (pure (f i) `switch` e')

count :: Now (Behavior Int)
count = magic id

