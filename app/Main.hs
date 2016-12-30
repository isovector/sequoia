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

type Prop = Prop' ()

magic :: Prop -> Engine -> Now (Behavior Prop)
magic baller _ = do
    clock <- getClock
    Just json  <- liftIO $ decode . toS <$> readFile "/home/bootstrap/Projects/bones/basic-anim.scon"
    let schema' = fromJSON json
    liftIO $ print schema'
    let Success schema = schema'

    return $ do
        now <- sample $ totalTime clock
        let skel = scale 0.1 $ doAnimation schema $ ((round $ now * 100) `mod` 300)
        return $ group [ skel
                       , filled red $ circle origin 5
                       , filled blue $ circle (center skel) 5
                       ]


makeBones :: Schema -> [Prop]
makeBones schema = toProp <$> schema ^. schemaEntity._head.entityObjInfo
  where
    toProp Bone{..} = traced white
                    $ polygon origin
                      [ rel 0 (toRealFloat $ _boneHeight / 2)
                      , rel (toRealFloat _boneWidth) 0
                      , rel 0 (toRealFloat $ (-_boneHeight) / 2)
                      ]

doAnimation :: Schema -> Int -> Prop
doAnimation schema frame =
  let bones = animate (head $ schema ^. schemaEntity._head.entityAnimation)
                      frame
      drawBone ResultBone{..} = move (rel _rbX (-_rbY))
                              . rotate (-_rbAngle)
   in case bones of
        Just x -> group . fmap (uncurry drawBone)
                        $ zip x (makeBones schema)
        Nothing -> traced red $ rect origin 10 10


main :: IO ()
main = withTexture "app/baller.png" $ \baller ->
         play (EngineConfig (640, 480) "hello" black) (magic baller) return

