module Main where

import Game.Sequoia
import Game.Sequoia.Color
import Game.Sequoia.Scene
import Game.Sequoia.Signal
import Game.Sequoia.Time
import Game.Sequoia.Types

config = EngineConfig
    { windowDimensions = (640, 480)
    , windowTitle = "sweet cuppin' cakes"
    }

movement :: Signal (Prop ())
movement = foldp update (filled red $ rect 20 20) elapsed
  where
    update dt p = move (mkRel (300 * dt) (300 * dt)) p

mainSig :: Signal [Prop ()]
mainSig = return <$> movement

main = run config mainSig

