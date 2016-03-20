module Game.Sequoia.Scene
    ( group
    , bake
    , ungroup
    , tags
    , tag
    , getTag
    , move
    , rotate
    , scale
    , teleport
    , rect
    , polygon
    , circle
    , filled
    , invisible
    ) where

import Control.Monad (join, guard)
import Data.Default (Default (), def)
import Data.SG.Geometry.TwoDim
import Data.SG.Shape
import Game.Sequoia.Color (rgba)
import Game.Sequoia.Types
import Game.Sequoia.Utils

group :: [Prop' a] -> Prop' a
group = GroupProp

tags :: (a -> a) -> Prop' a -> Prop' a
tags t (GroupProp ps)   = GroupProp $ map (tags t) ps
tags t (ShapeProp a f)  = ShapeProp (t a) f
tags t (BakedProp a fs) = BakedProp (t a) fs

tag :: a -> Prop' a -> Prop' a
tag = tags . const

getTag :: Default a => Prop' a -> a
getTag (GroupProp _)   = def
getTag (ShapeProp a _) = a
getTag (BakedProp a _) = a

bake :: Default a => [Prop' a] -> Prop' a
bake ps = BakedProp def . join $ map getForms ps
  where
    getForms (GroupProp ps)   = join $ map getForms ps
    getForms (ShapeProp _ f)  = return f
    getForms (BakedProp _ fs) = fs

-- |Groups don't have tags, so some algorithms might go pear-shaped if you
-- don't ungroup the scene first.
ungroup :: [Prop' a] -> [Prop' a]
ungroup = join . map ungroup'
  where
    ungroup' (GroupProp ps) = ungroup ps
    ungroup' a = return a

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

circle :: Pos -> Double -> Shape
circle = Circle

filled :: Default a => Color -> Shape -> Prop' a
filled c = ShapeProp def . Form (Solid c)

invisible :: Default a => Shape -> Prop' a
invisible = filled (rgba 0 0 0 0)

