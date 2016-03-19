module Game.Sequoia.Scene
    ( group
    , bake
    , tag
    , getTag
    , move
    , rotate
    , scale
    , rect
    , filled
    , sweepLine
    , center
    ) where

import Control.Monad (join, guard, liftM2)
import Data.SG.Geometry.TwoDim
import Data.SG.Shape
import Data.Maybe (isJust, fromJust)
import Game.Sequoia.Types
import Game.Sequoia.Utils

group :: [Prop' a] -> Prop' a
group = GroupProp

tag :: a -> Prop' a -> Prop' a
tag a (GroupProp ps)   = GroupProp $ map (tag a) ps
tag a (ShapeProp _ f)  = ShapeProp (Just a) f
tag a (BakedProp _ fs) = BakedProp (Just a) fs

getTag :: Prop' a -> Maybe a
getTag (GroupProp _)   = Nothing
getTag (ShapeProp a _) = a
getTag (BakedProp a _) = a

bake :: [Prop' a] -> Prop' a
bake ps = BakedProp Nothing . join $ map getForms ps
  where
    getForms (GroupProp ps)   = join $ map getForms ps
    getForms (ShapeProp _ f)  = return f
    getForms (BakedProp _ fs) = fs

move :: Rel -> Prop' a -> Prop' a
move = transform . liftShape . moveShape

rotate :: Double -> Prop' a -> Prop' a
rotate = transform . liftShape . rotateShape

scale :: Double -> Prop' a -> Prop' a
scale = transform . liftShape . scaleShape

transform :: (Form -> Form) -> Prop' a -> Prop' a
transform t (GroupProp ps)  = GroupProp   $ map (transform t) ps
transform t (ShapeProp a f) = ShapeProp a $ t f
transform t (BakedProp a f) = BakedProp a $ map t f

liftShape :: (Shape -> Shape) -> Form -> Form
liftShape t (Form fs s) = Form fs $ t s

rect :: Pos -> Double -> Double -> Shape
rect pos w h = Rectangle pos $ mapT (/2) (w, h)

filled :: Color -> Shape -> Prop' a
filled c = ShapeProp Nothing . Form (Solid c)

getShapes :: Prop' a -> [(Prop' a, Shape)]
getShapes   (GroupProp ps)   = join $ map getShapes ps
getShapes p@(ShapeProp _ f)  = return $ (p, getShape f)
getShapes p@(BakedProp _ fs) = zip (repeat p) $ map getShape fs

getShape :: Form -> Shape
getShape (Form _ s) = s

between :: Ord a => a -> a -> a -> Bool
between a b x = a <= x && x <= b

center :: Prop' a -> Pos
center (ShapeProp _ (Form _ s)) = shapeCentre s
-- TODO(sandy): do this better
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

