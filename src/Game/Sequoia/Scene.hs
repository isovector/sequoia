module Game.Sequoia.Scene
    ( group
    , bake
    , tag
    , getTag
    , move
    , rotate
    , scale
    , teleport
    , rect
    , polygon
    , filled
    , invisible
    ) where

import Control.Monad (join, guard)
import Data.SG.Geometry.TwoDim
import Data.SG.Shape
import Game.Sequoia.Color (rgba)
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

teleport :: Pos -> Prop' a -> Prop' a
teleport = transform . liftShape . teleportShape
  where
    teleportShape p s = s { shapeCentre = p }

transform :: (Form -> Form) -> Prop' a -> Prop' a
transform t (GroupProp ps)  = GroupProp   $ map (transform t) ps
transform t (ShapeProp a f) = ShapeProp a $ t f
transform t (BakedProp a f) = BakedProp a $ map t f

liftShape :: (Shape -> Shape) -> Form -> Form
liftShape t (Form fs s) = Form fs $ t s

rect :: Pos -> Double -> Double -> Shape
rect pos w h = Rectangle pos $ mapT (/2) (w, h)

polygon :: Pos -> [Rel] -> Shape
polygon = Polygon

filled :: Color -> Shape -> Prop' a
filled c = ShapeProp Nothing . Form (Solid c)

invisible :: Shape -> Prop' a
invisible = filled (rgba 0 0 0 0)

