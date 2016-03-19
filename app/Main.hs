module Main where

import Game.Sequoia
import Game.Sequoia.Color

config = EngineConfig
    { windowDimensions = (640, 480)
    , windowTitle = "sweet cuppin' cakes"
    }

type Prop = Prop' ()

movement :: Signal Prop
movement = foldp update (filled red $ rect 20 20) elapsed
  where
    update dt p = move (mkRel (300 * dt) (300 * dt)) p

mainSig :: Signal [Prop]
mainSig = return <$> movement

main = run config mainSig

