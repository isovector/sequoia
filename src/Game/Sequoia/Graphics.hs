{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Game.Sequoia.Graphics where

import           Data.Data
import qualified Data.Text as T
import           Game.Sequoia.Color (Color, black)
import           Game.Sequoia.Types
import           Graphics.Rendering.Cairo.Matrix (Matrix (..))


data FontWeight
  = LightWeight
  | NormalWeight
  | BoldWeight
  deriving (Show, Eq, Ord, Enum, Read, Data)

data FontStyle
  = NormalStyle
  | ObliqueStyle
  | ItalicStyle
  deriving (Show, Eq, Ord, Enum, Read, Data)

data Text = Text
  { textUTF8     :: !T.Text
  , textColor    :: !Color
  , textTypeface :: !T.Text
  , textHeight   :: !Double
  , textWeight   :: !FontWeight
  , textStyle    :: !FontStyle
  , textStroke   :: !(Maybe LineStyle)
  , textAlignment :: !Alignment
  , textVAlignment :: !VAlignment
  } deriving (Show, Eq, Data)

data Alignment
  = LeftAligned | CenterAligned
  deriving (Eq, Ord, Show, Enum, Bounded, Read, Data)

data VAlignment
  = TopVAligned | CenterVAligned
  deriving (Eq, Ord, Show, Enum, Bounded, Read, Data)

data Crop = Crop !Double !Double !Double !Double
  deriving (Show, Eq, Data)

data Element
  = CollageElement !Int !Int !(Maybe (Double, Double)) ![Form]
  | ImageElement !(Maybe Crop) !(Maybe Color) !FilePath
  | TextElement !Text
  deriving (Show, Eq, Data)

image :: FilePath -> Element
image src = ImageElement Nothing Nothing src

colorCorrectedImage :: FilePath -> Color -> Element
colorCorrectedImage src color = ImageElement Nothing (Just color) src

croppedImage :: Crop -> FilePath -> Element
croppedImage crop src = ImageElement (Just crop) Nothing src

data Form = Form
  { formTheta  :: !Double
  , formScaleX :: !Double
  , formScaleY :: !Double
  , formX      :: !Double
  , formY      :: !Double
  , formStyle  :: !FormStyle
  } deriving (Show, Eq, Data)

instance Semigroup Form where
  (<>) a b = group [a, b]

instance Monoid Form where
  mempty = group []
  mconcat = group

data FillStyle
  = Solid !Color
  | Texture !String
  deriving (Show, Eq, Ord, Read, Data)

data LineCap
  = FlatCap
  | RoundCap
  | PaddedCap
  deriving (Show, Eq, Enum, Ord, Read, Data)

data LineJoin
  = SmoothJoin
  | SharpJoin !Double
  | ClippedJoin
  deriving (Show, Eq, Ord, Read, Data)

data LineStyle = LineStyle
  { lineColor      :: !Color
  , lineWidth      :: !Double
  , lineCap        :: !LineCap
  , lineJoin       :: !LineJoin
  , lineDashing    :: ![Double]
  , lineDashOffset :: !Double
  } deriving (Show, Eq, Data)

defaultLine :: LineStyle
defaultLine = LineStyle
  { lineColor      = black
  , lineWidth      = 1
  , lineCap        = FlatCap
  , lineJoin       = SharpJoin 10
  , lineDashing    = []
  , lineDashOffset = 0
  }

solid :: Color -> LineStyle
solid color = defaultLine { lineColor = color }

dashed :: Color -> LineStyle
dashed color = defaultLine { lineColor = color, lineDashing = [8, 4] }

dotted :: Color -> LineStyle
dotted color = defaultLine { lineColor = color, lineDashing = [3, 3] }

data FormStyle
  = PathForm !LineStyle !Path
  | ShapeForm !(Either LineStyle FillStyle) !Shape
  | ElementForm !Element
  | GroupForm !(Maybe Matrix) ![Form]
  | CompositeForm CompositeStyle Form
  deriving (Show, Eq, Data)

data CompositeStyle = CompositeStyle
  { compositeAlpha :: Double
  }
  deriving (Show, Eq, Data)

deriving instance Data Matrix

form :: FormStyle -> Form
form style = Form
  { formTheta  = 0
  , formScaleX = 1
  , formScaleY = 1
  , formX      = 0
  , formY      = 0
  , formStyle  = style
  }

withAlpha :: Double -> Form -> Form
withAlpha alpha f = form $ CompositeForm (CompositeStyle alpha) f

fill :: FillStyle -> Shape -> Form
fill style = form . ShapeForm (Right style)

filled :: Color -> Shape -> Form
filled = fill . Solid

textured :: String -> Shape -> Form
textured = fill . Texture

outlined :: LineStyle -> Shape -> Form
outlined style shape = form (ShapeForm (Left style) shape)

traced :: LineStyle -> Path -> Form
traced style p = form (PathForm style p)

traced' :: Color -> Shape -> Form
traced' c = outlined defaultLine
  { lineColor = c
  , lineDashing = [8, 4]
  }

outlined' :: Color -> Shape -> Form
outlined' c = outlined defaultLine
  { lineColor = c
  }

sprite :: FilePath -> Form
sprite = toForm . image

toForm :: Element -> Form
toForm = form . ElementForm

blank :: Form
blank = group []

group :: [Form] -> Form
group = form . GroupForm Nothing

groupTransform :: Matrix -> [Form] -> Form
groupTransform matrix forms = form (GroupForm (Just matrix) forms)

rotate :: Double -> Form -> Form
rotate t f = group . pure $ f { formTheta = t + formTheta f }

scale :: Double -> Form -> Form
scale n f = group . pure $ f
          { formScaleX = n * formScaleX f
          , formScaleY = n * formScaleY f
          }

scaleXY :: Double -> Double -> Form -> Form
scaleXY x y f = group . pure $ f
              { formScaleX = x * formScaleX f
              , formScaleY = y * formScaleY f
              }

flipX :: Form -> Form
flipX f = group . pure $ f
        { formScaleX = negate $ formScaleX f
        }

move :: V2 -> Form -> Form
move (V2 rx ry) f = group . pure $ f { formX = rx + formX f, formY = ry + formY f }

collage :: Int -> Int -> [Form] -> Element
collage w h = CollageElement w h Nothing

centeredCollage :: Int -> Int -> [Form] -> Element
centeredCollage w h = CollageElement w h (Just (realToFrac w / 2, realToFrac h / 2))

fixedCollage :: Int -> Int -> (Double, Double) -> [Form] -> Element
fixedCollage w h (x, y) = CollageElement w h (Just (realToFrac w / 2 - x, realToFrac h / 2 - y))

type Path = [(Double, Double)]

path :: [V2] -> Path
path = fmap unpackV2

segment :: (Double, Double) -> (Double, Double) -> Path
segment p1 p2 = [p1, p2]

data Shape
  = PolygonShape !Path
  | RectangleShape !(Double, Double)
  | ArcShape !(Double, Double) !Double !Double !Double !(Double, Double)
  deriving (Show, Eq, Ord, Read, Data)

polygon :: [V2] -> Shape
polygon = PolygonShape . path

rect :: Double -> Double -> Shape
rect w h = RectangleShape (w, h)

square :: Double -> Shape
square n = rect n n

oval :: Double -> Double -> Shape
oval = ellipse

ellipse :: Double -> Double -> Shape
ellipse w h = polygon
              $ fmap (rad2rel . (* drad) . fromIntegral) [0..samples]
  where
    samples = 15 :: Int
    drad = 2 * pi / fromIntegral samples
    rad2rel rad = V2 (cos rad * w / 2) (sin rad * h / 2)

circle :: Double -> Shape
circle r = ArcShape (0, 0) 0 (2 * pi) r (1, 1)

arc :: V2 -> Double -> Double -> Shape
arc size theta phi = ArcShape (0, 0) theta phi 1 (unpackV2 size)

ngon :: Int -> Double -> Shape
ngon n r = PolygonShape (map (\i -> (r * cos (t * i), r * sin (t * i))) [0 .. fromIntegral (n - 1)])
  where
    m = fromIntegral n
    t = 2 * pi / m

