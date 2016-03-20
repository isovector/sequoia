module Game.Sequoia.Types
    ( Pos
    , Rel
    , Shape
    , Form (..)
    , Prop' (..)
    , Color (..)
    , FillStyle (..)
    , LineCap (..)
    , LineJoin (..)
    , LineStyle (..)
    , Style (..)
    , defaultLine
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

data LineCap = FlatCap
             | RoundCap
             | PaddedCap
             deriving (Show, Eq, Enum, Ord, Read)

data LineJoin = SmoothJoin
              | SharpJoin Double
              | ClippedJoin
              deriving (Show, Eq, Ord, Read)

data LineStyle = LineStyle
    { lineColor      :: Color
    , lineWidth      :: Double
    , lineCap        :: LineCap
    , lineJoin       :: LineJoin
    , lineDashing    :: [Double]
    , lineDashOffset :: Double
    } deriving (Show, Eq)

defaultLine :: LineStyle
defaultLine = LineStyle
    { lineColor = black
    , lineWidth = 1
    , lineCap = FlatCap
    , lineJoin = SharpJoin 10
    , lineDashing = []
    , lineDashOffset = 0
    }

data FillStyle = Solid Color
    deriving (Show, Eq)

data Style = Style (Maybe FillStyle) (Maybe LineStyle)
    deriving (Show, Eq)

data Form = Form Style Shape
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

