{-# LANGUAGE RecordWildCards #-}

module Game.Sequoia.Geometry
    ( sweepLine
    , sweepProp
    , overlapping
    , tryMove
    ) where

import Control.Monad (join, guard)
import Data.Foldable (toList)
import Data.SG.Geometry.TwoDim
import Data.SG.Shape
import Data.Maybe (isJust, fromJust)
import Game.Sequoia.Scene
import Game.Sequoia.Types

getShapes :: Piece a -> [(Piece a, Shape)]
getShapes p@(ShapePiece _ f)  = return $ (p, getShape f)
getShapes (StanzaPiece _ _)   = []

getShape :: Form -> Shape
getShape (Form _ s) = s

between :: Ord a => a -> a -> a -> Bool
between a b x = a <= x && x <= b

sweepLine :: [Piece a] -> Pos -> Rel -> [Piece a]
sweepLine ps pos rel' = do
    let line = Line2 pos rel'
        shapes = join $ map getShapes ps
    (p, s) <- shapes
    let mayIntersect = intersectLineShape line s
    guard $ isJust mayIntersect
    let intersect = fromJust mayIntersect
        p1 = fst intersect
        p2 = snd intersect
    guard $ between 0 1 p1 || between 0 1 p2
    return p

overlapping :: [Piece a] -> Prop' a -> [Piece a]
overlapping ps prop = do
    c <- toList prop
    let shapes = join $ map getShapes ps
        cshapes = getShapes c
    -- TODO(sandy): we can do a quick BB test instead of this fanout
    (p, s)  <- shapes
    (_, cs) <- cshapes
    -- TODO(sandy): overlap seems to think triangles are rects
    guard . isJust $ overlap s cs
    return p

sweepProp :: [Prop' a] -> Prop' a -> Rel -> [Piece a]
sweepProp pps prop rel' = do
    let ps = join $ fmap toList pps
    p <- toList prop
    let pos = centerOf p
    sweepLine ps pos rel' ++ overlapping ps (move rel' prop)

tryMove :: [Prop' a] -> [Prop' a] -> Prop' a -> Rel -> Prop' a
tryMove walls' floors ps rel'
  | rel' == origin = ps
  | otherwise =
    let walls = join $ fmap toList walls'
        pos = center ps
        -- TODO(sandy): make this a real sweep
        hitWalls = sweepLine walls pos rel'
        hitFloors = sweepProp floors ps rel'
        inClear = null hitWalls
        onFloor = null floors || not (null hitFloors)
        canMove = inClear && onFloor
     in if canMove
           then move rel' ps
           else ps

