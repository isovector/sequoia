{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.FRPNow
import Control.Applicative
import Game.Sequoia
import Game.Sequoia.Color

b :: Now (Behavior [Prop' ()])
b = pure . pure . return . filled red $ rect origin 50 50

main = flip play b $ EngineConfig (640, 480) "hello"
