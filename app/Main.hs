{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import Data.String.Conv (toS)
import Control.Lens
import Data.Spriter.Types
import Data.Spriter.Skeleton
import Data.Aeson (decode, fromJSON, Result (Success))
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Game.Sequoia
import Game.Sequoia.Color
import Game.Sequoia.Keyboard
import Data.Scientific (toRealFloat)

magic :: Engine -> Now (Behavior Element)
magic _ = do
    clock <- getClock
    Just json  <- liftIO $ decode . toS <$> readFile "/home/bootstrap/Projects/bones/basic-anim.scon"
    let schema' = fromJSON json
    liftIO $ print schema'
    let Success schema = schema'

    return $ do
        now <- sample $ totalTime clock
        let skel = scale 0.5 $ doAnimation schema $ ((round $ now * 100) `mod` 300)
        return $ centeredCollage 640 480
               [ move (V2 0 400) $ skel
               , toForm $ image "app/baller.png"
               ]


makeBones :: Schema -> [Form]
makeBones schema = toProp <$> schema ^. schemaEntity._head.entityObjInfo
  where
    toProp Bone{..} = traced' white
                    $ polygon
                      [ V2 0 (toRealFloat $ _boneHeight / 2)
                      , V2 (toRealFloat _boneWidth) 0
                      , V2 0 (toRealFloat $ (-_boneHeight) / 2)
                      ]

doAnimation :: Schema -> Int -> Form
doAnimation schema frame =
  let bones = animate (head $ schema ^. schemaEntity._head.entityAnimation)
                      frame
      drawBone ResultBone{..} = move (V2 _rbX $ -_rbY)
                              . rotate (-_rbAngle)
   in case bones of
        Just x -> group . fmap (uncurry drawBone)
                        $ zip x (makeBones schema)
        Nothing -> traced' red $ rect 10 10


main :: IO ()
main = play (EngineConfig (640, 480) "hello" black) magic return

