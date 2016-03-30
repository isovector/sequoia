{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Debug.Trace
import Game.Sequoia
import Game.Sequoia.Color
import Game.Sequoia.Stanza
import System.IO.Unsafe (unsafePerformIO)
import qualified Game.Sequoia.Keyboard as KB

config = EngineConfig
    { windowDimensions = (640, 480)
    , windowTitle = "sweet cuppin' cakes"
    }

type Prop = Prop' ()

{-# NOINLINE movement #-}
movement :: Signal Prop
(movement, movementAddr) = foldmp (filled red $ rect origin 10 10) $
    \p -> do
        dt  <- elapsed
        dir <- KB.arrows
        -- TODO(sandy): THERE IS A BUG IN SG CIRCLE RECT DETECTION
        return . tryMove otherBlock [] p
               . scaleRel dt $ dir * 300

magicSignal :: Signal Prop
magicSignal = do
    t  <- totalElapsed
    mv <- movement
    when (t > 3 && t < 3.1)
        . mail movementAddr
        . const
        . filled blue
        $ circle (center mv) 5
    return mv

totalElapsed :: Signal Time
totalElapsed = foldp (+) 0 elapsed

theFloor :: [Prop]
theFloor = return . traced white $ rect origin 220 400

otherBlock :: [Prop]
otherBlock = return
           . traced yellow
           $ rect (mkPos (-200) 100) 40 80

stanz :: [Prop]
stanz = return
      . StanzaProp
      . aligned RightAligned
      . monospace
      . color red
      $ toStanza "hello sequoia"

mainSig :: Signal [Prop]
mainSig = (: stanz ++ otherBlock ++ theFloor) <$> magicSignal

space :: Signal Bool
space = KB.keyPress KB.SpaceKey

main = run config mainSig

