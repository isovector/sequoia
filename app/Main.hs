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
movement = foldp update (filled red $ rect (mkPos 150 100) 20 20) $
    (,) <$> elapsed <*> KB.arrows
  where
    update (dt, dir) p = tryMove otherBlock p . scaleRel dt $ dir * 300

otherBlock :: [Prop]
otherBlock = return
           . filled violet
           $ polygon (mkPos 300 200) [ mkRel 0 (-40)
                                     , mkRel 20 40
                                     , mkRel (-20) 40
                                     ]

mainSig :: Signal [Prop]
mainSig = (: otherBlock) <$> movement

main = run config mainSig

