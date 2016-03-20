module Game.Sequoia.Types
    ( Pos
    , Rel
    , Shape
    , FillStyle (..)
    , Form (..)
    , Prop' (..)
    , Color (..)
    , mkPos
    , unpackPos
    , origin
    , mkRel
    , scaleRel
    , plusDir
    , posDif
    ) where

import Data.SG.Geometry
import Data.SG.Geometry.TwoDim
import Data.SG.Shape
import Game.Sequoia.Color

type Pos = Point2' Double
type Rel = Rel2' Double
type Shape = Shape' Double

data FillStyle = Solid Color
    deriving (Show, Eq)

data Form = Form FillStyle Shape
    deriving (Show, Eq)

data Prop' a = ShapeProp a  Form
             | BakedProp a [Form]
             | GroupProp [Prop' a]
             deriving (Show, Eq)

mkPos :: Double -> Double -> Pos
mkPos x y = Point2 (x, y)

mkRel :: Double -> Double -> Rel
mkRel x y = makeRel2 (x, y)

unpackPos :: Pos -> (Double, Double)
unpackPos (Point2 pos) = pos

origin :: Pos
origin = mkPos 0 0

posDif :: Pos -> Pos -> Rel
posDif = fromPt

