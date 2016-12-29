{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Eff
import Control.Eff.Reader.Lazy
import Control.Monad
import Game.Sequoia
import Game.Sequoia.Color
import Game.Sequoia.Keyboard

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

magic :: Prop -> Engine -> Now (Behavior Prop)
magic baller _ = do
    clock    <- getClock
    keyboard <- getKeyboard

    return $ do
        now <- sample $ totalTime clock
        return $ group [rotate now baller]

main :: IO ()
main = withTexture "app/baller.png" $ \baller ->
         play (EngineConfig (640, 480) "hello" black) (magic baller) return

