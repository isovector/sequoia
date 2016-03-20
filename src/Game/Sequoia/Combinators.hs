module Game.Sequoia.Combinators
    ( focusing
    , eraser
    ) where

import Data.List (nub)
import Game.Sequoia.Geometry
import Game.Sequoia.Scene
import Game.Sequoia.Signal
import Game.Sequoia.Types

focusing :: Prop' a -> [Prop' a] -> [Prop' a]
focusing c = map (move . posDif origin $ center c)

eraser :: Eq a
       => Signal [Prop' a]
       -> (Prop' a -> Bool)
       -> Signal (Prop' a)
       -> Signal [Prop' a]
eraser sps f sdozer =
    let remSig = foldp doze [] ((,) <$> sps <*> sdozer)
     in filterOutIdx <$> remSig <*> sps
  where
    doze (ps', dozer) rem =
        let ps    = filterOutIdx rem ps'
            hit   = overlapping ps dozer
            dozed = nub $ filter f hit
         in if null dozed
               then rem
               else rem ++ idxsOf ps' dozed

    filterOutIdx :: [Int] -> [a] -> [a]
    filterOutIdx idx = map snd . filter (flip notElem idx . fst) . zip [0..]

    idxsOf :: Eq a => [a] -> [a] -> [Int]
    idxsOf all which = map fst . filter (flip elem which . snd) $ zip [0..] all

