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
    return $ return $ centeredCollage 640 480 []

main :: IO ()
main = play (EngineConfig (640, 480) "hello" black) magic return

