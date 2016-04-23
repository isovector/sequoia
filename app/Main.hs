{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Eff
import Control.Eff.Lift
import Control.Eff.Reader.Lazy
import Control.Monad
import Data.Void
import Debug.Trace
import Game.Sequoia
import Game.Sequoia.Color
import Game.Sequoia.Keyboard
import Game.Sequoia.Utils

type Prop = Prop' ()

square :: ( Member (Reader (Behavior Time)) r
          , Member (Reader (Behavior [Key])) r
          )
       => Eff r (Now (Behavior Prop, Address (Prop -> Prop)))
square = do
    (clock :: Behavior Time) <- ask
    (keys :: Behavior [Key]) <- ask

    return $ do
        foldmp (filled red $ rect origin 50 50) $ \sq -> do
            dt   <- sample clock
            dpos <- sample $ arrows keys
            return $ move (scaleRel (300 * dt) dpos) sq

magic :: Engine -> Now (Behavior (Prop' ()))
magic engine = do
    clock      <- getElapsedClock
    keyboard   <- getKeyboard
    (sq, addr) <- run . flip runReader clock
                 . flip runReader keyboard
                 $ square
    poll $ do
        spaceDown <- sample $ isDown keyboard SpaceKey
        aDown     <- sample $ isDown keyboard AKey
        when spaceDown . sync $ addr (refill blue)
        when aDown     . sync $ addr (refill green)
    return sq

main = play (EngineConfig (640, 480) "hello") magic $ \sq -> do
    return $ fmap return sq
