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
       => [Prop]
       -> Eff r (Now (Behavior Prop, (Prop -> Prop) -> IO ()))
square walls = do
    (clock :: Behavior Time) <- ask
    (keys :: Behavior [Key]) <- ask

    return $ do
        foldmp (group [ filled red $ rect origin 50 50
                      , filled red $ circle (mkPos 0 $ -25) 25
                      ]) $ \sq -> do
            dt   <- sample clock
            dpos <- sample $ arrows keys
            return $ tryMove walls [] sq (scaleRel (300 * dt) dpos)

magic :: Engine -> Now (Behavior Prop)
magic engine = do
    clock    <- getClock
    keyboard <- getKeyboard
    let wall = filled blue $ circle (mkPos 35 35) 25
    (sq, addr) <- run . flip runReader (deltaTime clock)
                      . flip runReader keyboard
                      $ square [wall]
    poll $ do
        spaceDown <- sample $ isDown keyboard SpaceKey
        aDown     <- sample $ isDown keyboard AKey
        when spaceDown . sync $ addr (refill blue)
        when aDown     . sync $ addr (refill green)

    return $ do
        sq' <- sq
        return $ group [sq', wall]

main = play (EngineConfig (640, 480) "hello") magic return

