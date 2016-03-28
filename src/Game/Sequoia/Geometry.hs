module Game.Sequoia.Geometry
    ( center
    , sweepLine
    , sweepProp
    , overlapping
    , tryMove
    ) where

import Control.Monad (join, guard)
import Data.SG.Geometry.TwoDim
import Data.SG.Shape
import Data.Maybe (isJust, fromJust)
import Game.Sequoia.Scene
import Game.Sequoia.Types
import Game.Sequoia.Utils

getShapes :: Prop' a -> [(Prop' a, Shape)]
getShapes   (GroupProp ps)   = join $ map getShapes ps
getShapes p@(ShapeProp _ f)  = return $ (p, getShape f)
getShapes p@(BakedProp _ fs) = zip (repeat p) $ map getShape fs
getShapes (StanzaProp _)     = []

getShape :: Form -> Shape
getShape (Form _ s) = s

between :: Ord a => a -> a -> a -> Bool
between a b x = a <= x && x <= b

center :: Prop' a -> Pos
center (ShapeProp _ (Form _ s)) = shapeCentre s
-- TODO(sandy): do better than this
center _ = error "you can't get the center of NOTHING"

sweepLine :: [Prop' a] -> Pos -> Rel -> [Prop' a]
sweepLine ps pos rel =
    let line = Line2 pos rel
        shapes = join $ map getShapes ps
     in do
            (p, s) <- shapes
            let mayIntersect = intersectLineShape line s
            guard $ isJust mayIntersect
            let intersect = fromJust mayIntersect
                p1 = fst intersect
                p2 = snd intersect
            guard $ between 0 1 p1 || between 0 1 p2
            return p

overlapping :: [Prop' a] -> Prop' a -> [Prop' a]
overlapping ps c =
    let shapes = join $ map getShapes ps
        cshapes = getShapes c
     in do
           -- TODO(sandy): we can do a quick BB test instead of this fanout
           (p, s)  <- shapes
           (_, cs) <- cshapes
           -- TODO(sandy): overlap seems to think triangles are rects
           guard . isJust $ overlap s cs
           return p

sweepProp :: [Prop' a] -> Prop' a -> Rel -> [Prop' a]
sweepProp ps p rel =
    let pos = center p
     in sweepLine ps pos rel ++ overlapping ps (move rel p)

tryMove :: [Prop' a] -> [Prop' a] -> Prop' a -> Rel -> Prop' a
tryMove walls floors p rel =
    let pos = center p
        -- TODO(sandy): make this a real sweep
        hitWalls = sweepLine walls pos rel
        hitFloors = sweepProp floors p rel
        inClear = null hitWalls
        onFloor = null floors || not (null hitFloors)
        canMove = inClear && onFloor
     in if canMove
           then move rel p
           else p

