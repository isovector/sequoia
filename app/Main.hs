{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import Game.Sequoia
import Game.Sequoia.Color
import Game.Sequoia.Text

magic :: Engine -> Now (Behavior Element)
magic _ = do
  pure . pure
       . centeredCollage 640 480
       . return
       . toForm
       . text
       . height 8
       . color white
       . stroke (defaultLine { lineColor = black, lineWidth = 4 } )
       $ toText "hello"

main :: IO ()
main = play (EngineConfig (640, 480) "hello" white) magic return

