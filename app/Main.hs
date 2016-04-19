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
import Game.Sequoia.Utils

square :: Member (Reader (Behavior Time)) r
       => Eff r (Behavior (Prop' ()))
square = do
    clock <- trace "asking" ask
    return . join $
        foldp f (filled red $ rect origin 50 50) clock
  where
    f dt sq = trace (show $ center sq) $ move (mkRel 0 $ dt * 100) sq

main = play (EngineConfig (640, 480) "hello") $ do
    clock <- getClock
    let sq = run $ runReader square clock
    return $ fmap return sq
