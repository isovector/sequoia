{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Eff
import Control.Eff.Lift
import Control.Eff.Reader.Lazy
import Control.FRPNow
import Control.Monad
import Data.Void
import Debug.Trace
import Game.Sequoia
import Game.Sequoia.Color
import Game.Sequoia.Keyboard
import Game.Sequoia.Utils

square :: ( Member (Reader (Behavior Time)) r
          , Member (Reader (Behavior [Key])) r
          )
       => Eff r (Behavior (Behavior (Prop' ())))
square = do
    (clock :: Behavior Time) <- ask
    (keys :: Behavior [Key]) <- ask
    return $ foldp f (filled red $ rect origin 50 50)
           $ (,) <$> clock <*> arrows keys
  where
    f (dt, keys) sq = move (keys * 10) sq

magic = do
    schedule <- getScheduler
    clock    <- getElapsedClock schedule
    keyboard <- getKeyboard schedule
    let sq = run . flip runReader clock
                 . flip runReader keyboard
                 $ square
    sample sq

main = play (EngineConfig (640, 480) "hello") magic $ \sq -> do
    return $ fmap return sq
