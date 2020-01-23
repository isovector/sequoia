{-# LANGUAGE OverloadedStrings #-}

module Game.Sequoia.Text where

import Game.Sequoia.Color (Color, black)
import Game.Sequoia.Graphics
import qualified Data.Text as T

{-| Creates the default text. By default the text is black sans-serif
    with a height of 14pt. -}
defaultText :: Text
defaultText = Text
  { textUTF8     = ""
  , textColor    = black
  , textTypeface = "sans-serif"
  , textHeight   = 14
  , textWeight   = NormalWeight
  , textStyle    = NormalStyle
  , textStroke   = Nothing
  , textAlignment = LeftAligned
  , textVAlignment = TopVAligned
  }

{-| Creates a text from a string. -}
toText :: T.Text -> Text
toText utf8 = defaultText { textUTF8 = utf8 }

stringText :: String -> Text
stringText = toText . T.pack

{-| Creates a text element from a string. -}
plainText :: T.Text -> Element
plainText utf8 = text $ toText utf8

{-| Creates a text element from any showable type, defaulting to
    the monospace typeface. -}
asText :: Show a => a -> Element
asText val = text $ monospace $ toText $ T.pack $ show val

leftAligned :: Text -> Text
leftAligned t = t
  { textAlignment = LeftAligned
  }

centerAligned :: Text -> Text
centerAligned t = t
  { textAlignment = CenterAligned
  }

topVAligned :: Text -> Text
topVAligned t = t
  { textVAlignment = TopVAligned
  }

centerVAligned :: Text -> Text
centerVAligned t = t
  { textVAlignment = CenterVAligned
  }

{-| Creates an element from a text. -}
text :: Text -> Element
text = TextElement

{- TODO:
justified
righted
underline
strikeThrough
overline
-}

{-| Sets the weight of a piece of text to bold. -}
bold :: Text -> Text
bold txt = txt { textWeight = BoldWeight }

{-| Sets the weight of a piece of text to light. -}
light :: Text -> Text
light txt = txt { textWeight = LightWeight }

{-| Sets the slant of a piece of text to italic. -}
italic :: Text -> Text
italic txt = txt { textStyle = ItalicStyle }

{-| Sets the slant of a piece of text to oblique. -}
oblique :: Text -> Text
oblique txt = txt { textStyle = ObliqueStyle }

{-| Sets the color of a piece of text. -}
color :: Color -> Text -> Text
color col txt = txt { textColor = col }

{-| Sets the typeface of the text to monospace. -}
monospace :: Text -> Text
monospace txt = txt { textTypeface = "monospace" }

{-| Sets the typeface of the text. -}
typeface :: T.Text -> Text -> Text
typeface face txt = txt { textTypeface = face }

{-| Sets the size of a text noticeably large. -}
header :: Text -> Text
header = height 32

{-| Sets the size of a piece of text. -}
height :: Double -> Text -> Text
height size txt = txt { textHeight = size }

stroke :: LineStyle -> Text -> Text
stroke ls txt = txt { textStroke = Just ls }
