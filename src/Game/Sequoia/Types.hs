module Game.Sequoia.Types
    ( Pos
    , Rel
    , Shape
    , FillStyle (..)
    , Form (..)
    , Prop (..)
    , Color (..)
    , mkPos
    , unpackPos
    , origin
    ) where

import Data.SG.Geometry.TwoDim
import Data.SG.Shape
import Game.Sequoia.Color

type Pos = Point2' Double
type Rel = Rel2' Double
type Shape = Shape' Double

data FillStyle = Solid Color

data Form = Form FillStyle Shape

data Prop a = ShapeProp (Maybe a)  Form
            | BakedProp (Maybe a) [Form]
            | GroupProp [Prop a]

mkPos :: Double -> Double -> Pos
mkPos x y = Point2 (x, y)

unpackPos :: Pos -> (Double, Double)
unpackPos (Point2 pos) = pos

origin :: Pos
origin = mkPos 0 0

