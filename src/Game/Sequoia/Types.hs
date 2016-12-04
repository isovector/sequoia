{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PatternSynonyms   #-}

module Game.Sequoia.Types
    ( Tree
    , pattern Leaf
    , pattern Branch
    , Prop'
    , Pos
    , Rel
    , Shape
    , Form (..)
    , Piece (..)
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
    , rel
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

import Control.Monad.Free
import Data.SG.Geometry
import Data.SG.Geometry.TwoDim
import Data.SG.Shape
import Data.SG.Vector
import Game.Sequoia.Color
import Data.Text (Text)

type Tree = Free []
pattern Leaf a = Pure a
pattern Branch a = Free a

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

data Piece a = ShapePiece  a Form
             | StanzaPiece a Stanza
             deriving (Show, Eq)

type Prop' a = Tree (Piece a)

mkPos :: Double -> Double -> Pos
mkPos x y = Point2 (x, y)

rel :: Double -> Double -> Rel
rel x y = makeRel2 (x, y)

unpackPos :: Pos -> (Double, Double)
unpackPos (Point2 pos) = pos

posDif :: (Geometry rel pt ln, Num a) => pt a -> pt a -> rel a
posDif = fromPt

normalize :: (Coord p, VectorNum p, Ord a, Floating a) => p a -> p a
normalize = unitVector

distance :: (VectorNum pt, Coord pt, Floating a) => pt a -> pt a -> a
distance = distFrom

dot :: (Coord p, Num a) => p a -> p a -> a
dot = dotProduct

