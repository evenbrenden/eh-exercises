-- Handling Correctly Spelled Words

{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-full-laziness #-}

module Correct where

import           Control.Monad.ST
import           Data.Foldable                  ( for_ )
import           Data.Hashable                  ( hash )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Unsafe              as TU
import qualified Data.Vector.Unboxed.Mutable   as MVec
import           Prelude                 hiding ( length
                                                , read
                                                , words
                                                )
import           Types

{-# INLINE editDistance #-}
editDistance :: Text -> Text -> Int
editDistance stringA stringB = runST $ do
    let aLen = T.length stringA
        bLen = T.length stringB
        {-# INLINE lookupIndex #-}
        lookupIndex x y = (y * (aLen + 1)) + x
    cache <- MVec.new $ (aLen + 1) * (bLen + 1)
    for_ [0 .. aLen] $ \idx -> MVec.write cache (lookupIndex idx 0) idx
    for_ [0 .. bLen] $ \idx -> MVec.write cache (lookupIndex 0 idx) idx
    let
        columnCost !idxA !textIdxA
            | idxA > aLen = pure ()
            | otherwise = do
                let (TU.Iter !a' !textIdxA') = TU.iter stringA textIdxA
                    {-# INLINE rowCost #-}
                    rowCost !idxB !textIdxB
                        | idxB > bLen = pure ()
                        | otherwise = do
                            let (TU.Iter !b' !textIdxB') =
                                    TU.iter stringB textIdxB
                                cost = if a' == b' then 0 else 1
                            insertCost <- (1 +) <$> MVec.read
                                cache
                                (lookupIndex (idxA - 1) idxB)
                            deleteCost <- (1 +) <$> MVec.read
                                cache
                                (lookupIndex idxA (idxB - 1))
                            swapCost <- (cost +) <$> MVec.read
                                cache
                                (lookupIndex (idxA - 1) (idxB - 1))
                            let {-# INLINE newCost #-}
                                newCost =
                                    min swapCost $ min insertCost deleteCost
                            MVec.write cache (lookupIndex idxA idxB) newCost
                            rowCost (idxB + 1) (textIdxB + textIdxB')
                rowCost 1 0
                columnCost (idxA + 1) (textIdxA + textIdxA')
    columnCost 1 0
    MVec.read cache (lookupIndex aLen bLen)

spellcheckWord :: (Text -> Bool) -> [Text] -> Int -> Text -> [SuggestedMatch]
spellcheckWord correct dictionary threshold word = if correct word
    then []
    else getSuggestions dictionary []
  where
    getSuggestions [] suggestions = suggestions
    getSuggestions (dictWord : dict) suggestions
        | distance > threshold = getSuggestions dict suggestions
        | otherwise            = getSuggestions dict (suggestion : suggestions)
      where
        distance   = editDistance dictWord word
        suggestion = SuggestedMatch dictWord word distance

spellcheck :: [Text] -> Int -> [Text] -> [SuggestedMatch]
spellcheck dictionary threshold =
    let hashed = (,) <*> hash <$> dictionary
        correct word = any ((hash word ==) . snd) hashed
    in  concatMap (spellcheckWord correct dictionary threshold)
