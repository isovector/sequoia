{-# LANGUAGE LambdaCase #-}
module Game.Sequoia.Scene
    ( group
    , tagging
    , tag
    , tags
    , findTag
    , move
    , rotate
    , scale
    , teleport
    , rect
    , polygon
    , circle
    , filled
    , refill
    , invisible
    , styled
    , traced
    , withTexture
    ) where

import           Control.Arrow (first, second)
import           Control.Monad (ap)
import           Data.Default (Default (), def)
import           Data.Foldable (toList)
import           Data.SG.Shape
import           Game.Sequoia.Types
import           Game.Sequoia.Utils
import qualified Graphics.Rendering.Cairo as Cairo

group :: [Prop' a] -> Prop' a
group = Branch

tagging :: (a -> a) -> Prop' a -> Prop' a
tagging t = fmap $ \case
    ShapePiece  a f -> ShapePiece  (t a) f
    StanzaPiece a f -> StanzaPiece (t a) f

tag :: Piece a -> a
tag (ShapePiece  a _) = a
tag (StanzaPiece a _) = a

tags :: Prop' a -> [a]
tags = fmap tag . toList

findTag :: (a -> Bool) -> (a -> b) -> Prop' a -> [(Prop' a, b)]
findTag f t = map (first Leaf)
            . map (second t)
            . filter (f . snd)
            . map (ap (,) tag)
            . toList

move :: Rel -> Prop' a -> Prop' a
move rel' = transform (liftShape $ moveShape rel') moveStanza
  where
    moveStanza s = s { stanzaCentre = plusDir (stanzaCentre s) rel' }

rotate :: Double -> Prop' a -> Prop' a
rotate theta = transform (liftShape $ rotateShape theta)
             -- TODO(sandy): we can support this in drawing code
             $ error "unable to rotate stanza"

scale :: Double -> Prop' a -> Prop' a
scale s = transform (liftShape $ scaleShape s)
        -- TODO(sandy): we can support this in drawing code
        $ error "unable to scale stanza"

teleport :: Pos -> Prop' a -> Prop' a
teleport pos = transform (liftShape $ teleportShape pos) teleportStanza
  where
    teleportShape p s  = s { shapeCentre  = p }
    teleportStanza s   = s { stanzaCentre = pos }

transform :: (Form -> Form) -> (Stanza -> Stanza) -> Prop' a -> Prop' a
transform sh st = fmap $ \case
    ShapePiece a f  -> ShapePiece  a $ sh f
    StanzaPiece a s -> StanzaPiece a $ st s

liftShape :: (Shape -> Shape) -> Form -> Form
liftShape t (Form fs s) = Form fs $ t s

rect :: Pos -> Double -> Double -> Shape
rect pos w h = Rectangle pos $ mapT (/2) (w, h)

polygon :: Pos -> [Rel] -> Shape
polygon = Polygon

circle :: Pos -> Double -> Shape
circle = Circle

toShape :: Default a => Maybe FillStyle -> Maybe LineStyle -> Shape -> Prop' a
toShape fs ls = Leaf . ShapePiece def . Form (Style fs ls)

refill :: Color -> Prop' a -> Prop' a
refill c = transform
    (\(Form _ s) -> Form (Style (Just $ Solid c) Nothing) s)
    (\s -> s { stanzaColor = c })

filled :: Default a => Color -> Shape -> Prop' a
filled c = toShape (Just $ Solid c) Nothing

invisible :: Default a => Shape -> Prop' a
invisible = toShape Nothing Nothing

styled :: Default a => Color -> LineStyle -> Shape -> Prop' a
styled c ls = toShape (Just $ Solid c) (Just ls)

traced :: Default a => Color -> Shape -> Prop' a
traced c = toShape Nothing
                   (Just $ defaultLine { lineColor = c
                                       , lineDashing = [8, 4]
                                       } )

withTexture :: Default a => FilePath -> (Prop' a -> IO b) -> IO b
withTexture path f = Cairo.withImageSurfaceFromPNG path $ \surface -> do
  width  <- fromIntegral <$> Cairo.imageSurfaceGetWidth surface
  height <- fromIntegral <$> Cairo.imageSurfaceGetHeight surface
  f . Leaf
    . ShapePiece def
    . Form (Textured surface width height)
    $ rect origin width height

