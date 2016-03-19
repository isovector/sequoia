module Main where

import Game.Sequoia
import Game.Sequoia.Color
import qualified Game.Sequoia.Keyboard as KB

config = EngineConfig
    { windowDimensions = (640, 480)
    , windowTitle = "sweet cuppin' cakes"
    }

type Prop = Prop' ()

movement :: Signal Prop
movement = foldp update (filled red $ rect origin 20 20) $
    (,) <$> elapsed <*> KB.wasd
  where
    update (dt, dpos) p = move (scaleRel dt $ dpos * 300) p

mainSig :: Signal [Prop]
mainSig = return <$> movement

main = run config mainSig

