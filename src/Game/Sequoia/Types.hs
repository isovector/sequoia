{-# LANGUAGE PatternSynonyms   #-}

module Game.Sequoia.Types
    ( pattern V2
    , pattern V3
    , V2
    , V3
    , _x
    , _y
    , _z
    , distance
    , norm
    , signorm
    , unpackV2
    , unpackV3
    , dot
    ) where

import           Linear.Metric (distance, norm, signorm, dot)
import qualified Linear.V2 as LV2
import           Linear.V2 hiding (V2 ())
import qualified Linear.V3 as LV3
import           Linear.V3 hiding (V3 ())

type V2 = LV2.V2 Double
type V3 = LV3.V3 Double

unpackV2 :: V2 -> (Double, Double)
unpackV2 (LV2.V2 x y) = (x, y)

unpackV3 :: V3 -> (Double, Double, Double)
unpackV3 (LV3.V3 x y z) = (x, y, z)

