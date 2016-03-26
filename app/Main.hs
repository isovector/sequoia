module Main where

import Debug.Trace
import Game.Sequoia
import Game.Sequoia.Color
import System.IO.Unsafe (unsafePerformIO)
import qualified Game.Sequoia.Keyboard as KB

config = EngineConfig
    { windowDimensions = (640, 480)
    , windowTitle = "sweet cuppin' cakes"
    }

type Prop = Prop' ()

movement :: Signal Prop
movement = foldp update (filled red $ rect origin 10 10) $
    (,) <$> elapsed <*> KB.arrows
  where
    update (dt, dir) p = tryMove otherBlock theFloor p
                       . scaleRel dt $ dir * 300

theFloor :: [Prop]
theFloor = return . traced white $ rect origin 400 400

otherBlock :: [Prop]
otherBlock = return
           . traced yellow
           $ rect (mkPos (-200) 100) 40 80

sigA :: Signal Int
sigB :: Signal Int
(sigA, addrA) = newMailbox "a" 0
(sigB, addrB) = newMailbox "b" 0

mainSig :: Signal [Prop]
mainSig = (: otherBlock) <$> movement

space :: Signal Bool
space = KB.keyPress KB.SpaceKey

main = run config mainSig

