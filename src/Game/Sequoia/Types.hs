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
    , FontStyle (..)
    , FontWeight (..)
    , StanzaAlignment (..)
    , Stanza (..)
    , defaultLine
    , mkPos
    , unpackPos
    , origin
    , mkRel
    , scaleRel
    , plusDir
    , posDif
    , mag
    , magSq
    , dot
    , normalize
    , distance
    , getX
    , getY
    ) where

import Data.SG.Geometry
import Data.SG.Geometry.TwoDim
import Data.SG.Shape
import Data.SG.Vector
import Game.Sequoia.Color
import Data.Text (Text)

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

data FontWeight = LightWeight
                | NormalWeight
                | BoldWeight
                deriving (Show, Eq, Ord, Enum, Read)

data FontStyle = NormalStyle
               | ObliqueStyle
               | ItalicStyle
               deriving (Show, Eq, Ord, Enum, Read)

data StanzaAlignment = LeftAligned
                     | Centered
                     | RightAligned
                     deriving (Show, Eq, Ord, Enum, Read)

data Stanza = Stanza
    { stanzaUTF8      :: Text
    , stanzaColor     :: Color
    , stanzaTypeface  :: Text
    , stanzaHeight    :: Double
    , stanzaWeight    :: FontWeight
    , stanzaStyle     :: FontStyle
    , stanzaCentre    :: Pos
    , stanzaAlignment :: StanzaAlignment
    } deriving (Show, Eq)

data FillStyle = Solid Color
    deriving (Show, Eq)

data Style = Style (Maybe FillStyle) (Maybe LineStyle)
    deriving (Show, Eq)

data Form = Form Style Shape
    deriving (Show, Eq)

data Prop' a = ShapeProp a  Form
             | BakedProp a [Form]
             | GroupProp [Prop' a]
             | StanzaProp Stanza
             deriving (Show, Eq)

mkPos :: Double -> Double -> Pos
mkPos x y = Point2 (x, y)

mkRel :: Double -> Double -> Rel
mkRel x y = makeRel2 (x, y)

unpackPos :: Pos -> (Double, Double)
unpackPos (Point2 pos) = pos

posDif :: Pos -> Pos -> Rel
posDif = fromPt

normalize :: Rel -> Rel
normalize = unitVector

distance :: Pos -> Pos -> Double
distance = distFrom

dot :: Rel -> Rel -> Double
dot = dotProduct

