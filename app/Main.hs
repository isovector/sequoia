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

sig :: Signal Int
addr :: Address Int
(sig, addr) = unsafePerformIO $ mailboxs (+) 0

movement :: Signal Prop
movement = foldp update (filled red $ circle origin 10) $
    (,,) <$> elapsed <*> KB.arrows <*> sig
  where
    update (dt, dir, v) p = mailing addr 1
                          . tryMove otherBlock p
                          . scaleRel dt $ dir * 300

otherBlock :: [Prop]
otherBlock = return
           . traced yellow
           $ polygon (mkPos (-200) 100) [ mkRel 0 (-40)
                                        , mkRel 20 40
                                        , mkRel (-20) 40
                                        ]

mainSig :: Signal [Prop]
mainSig = (: otherBlock) <$> movement

space :: Signal Bool
space = KB.keyPress KB.SpaceKey

main = run config mainSig

