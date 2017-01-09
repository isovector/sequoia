{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode, fromJSON, Result (Success))
import Data.List (sortBy)
import Data.Map.Lens
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Scientific (toRealFloat)
import Data.Spriter.Skeleton
import Data.Spriter.Types
import Data.String.Conv (toS)
import Game.Sequoia
import Game.Sequoia.Color
import Game.Sequoia.Keyboard

magic :: Engine -> Now (Behavior Element)
magic _ = do
  clock <- getClock
  return $ do
    now <- sample $ totalTime clock
    let r = (sin now + 1) / 2
        g = (sin (now + pi * 2 / 3) + 1) / 2
        b = (sin (now + pi * 4 / 3) + 1) / 2
    return $ centeredCollage 640 480
           . return
           . toForm
           $ colorCorrectedImage "app/masktest.png" $ rgb r g b

main :: IO ()
main = play (EngineConfig (640, 480) "hello" black) magic return

