{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import Data.Ord (comparing)
import Data.List (sortBy)
import Data.Monoid ((<>))
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
    Just json  <- liftIO $ decode . toS <$> readFile "/home/bootstrap/Projects/bones/skinned.scon"
    let schema' = fromJSON json
    liftIO $ print schema'
    let Success schema = schema'

    return $ do
        now <- sample $ totalTime clock
        let skel = scale 0.5 $ doBonewise schema $ ((round $ now * 100) `mod` 300)
            anim = scale 0.5 $ doSpritewise schema $ ((round $ now * 100) `mod` 300)
        return $ centeredCollage 640 480
               [ move (V2 0 (-100)) anim
               , move (V2 0 (-100)) skel
               ]

makeSprites :: Schema -> [(Form, File)]
makeSprites schema = toProp
                 <$> schema ^. schemaFolder._head.folderFile
  where
    toProp f@File{..} = (sprite $ "/home/bootstrap/spriter-test/" <> _fileName, f)



makeBones :: Schema -> [Form]
makeBones schema = toProp <$> schema ^. schemaEntity._head.entityObjInfo
  where
    toProp Bone{..} = traced' white
                    $ polygon
                      [ V2 0 (toRealFloat $ _boneHeight / 2)
                      , V2 (toRealFloat _boneWidth) 0
                      , V2 0 (toRealFloat $ (-_boneHeight) / 2)
                      ]

doSpritewise :: Schema -> Int -> Form
doSpritewise schema frame =
  let bones = animate (head $ schema ^. schemaEntity._head.entityAnimation)
                      frame
      drawBone ResultBone{..} (form, _) =
        move (V2 _rbX $ -_rbY) . rotate (-_rbAngle)
                               $ form
   in case bones of
        Just x -> group . fmap (uncurry drawBone)
                        $ zip (sortBy (comparing $ (fmap . fmap) _boneObjFile _rbObj) $ filter (not . isBone) x) (makeSprites schema)
        Nothing -> traced' red $ rect 10 10

doBonewise :: Schema -> Int -> Form
doBonewise schema frame =
  let bones = animate (head $ schema ^. schemaEntity._head.entityAnimation)
                      frame
      drawBone ResultBone{..} = move (V2 _rbX $ -_rbY)
                              . rotate (-_rbAngle)
   in case bones of
        Just x -> group . fmap (uncurry drawBone)
                        $ zip (filter isBone x) (makeBones schema)
        Nothing -> traced' red $ rect 10 10


main :: IO ()
main = play (EngineConfig (640, 480) "hello" black) magic return

