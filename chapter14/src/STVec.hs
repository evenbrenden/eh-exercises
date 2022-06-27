{-# OPTIONS_GHC -fno-full-laziness #-}

module STVec where

import           Control.Monad
import           Control.Monad.ST
import           Data.Foldable                  ( for_ )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Vector.Mutable           as MVec
import           Prelude                 hiding ( length
                                                , read
                                                , words
                                                )
import           Types

editDistance :: Text -> Text -> Int
editDistance stringA stringB = runST $ do
    let aLen = T.length stringA
        bLen = T.length stringB
        as   = zip [1 ..] (T.unpack stringA)
        bs   = zip [1 ..] (T.unpack stringB)
        lookupIndex x y = (y * (aLen + 1)) + x
    cache <- MVec.new $ (aLen + 1) * (bLen + 1)
    for_ [0 .. aLen] $ \idx -> MVec.write cache (lookupIndex idx 0) idx
    for_ [0 .. bLen] $ \idx -> MVec.write cache (lookupIndex 0 idx) idx
    for_ as $ \(idxA, charA) -> do
        for_ bs $ \(idxB, charB) -> do
            let cost = if charA == charB then 0 else 1
            insertCost <- (1 +)
                <$> MVec.read cache (lookupIndex (idxA - 1) idxB)
            deleteCost <- (1 +)
                <$> MVec.read cache (lookupIndex idxA (idxB - 1))
            swapCost <- (cost +)
                <$> MVec.read cache (lookupIndex (idxA - 1) (idxB - 1))
            MVec.write cache (lookupIndex idxA idxB)
                $ minimum [insertCost, deleteCost, swapCost]
    MVec.read cache $ lookupIndex aLen bLen

spellcheckWord :: [Text] -> Int -> Text -> [SuggestedMatch]
spellcheckWord dictionary threshold word = getSuggestions dictionary []
  where
    getSuggestions [] suggestions = suggestions
    getSuggestions (dictWord : dict) suggestions
        | distance == 0        = []
        | distance > threshold = getSuggestions dict suggestions
        | otherwise            = getSuggestions dict (suggestion : suggestions)
      where
        distance   = editDistance dictWord word
        suggestion = SuggestedMatch dictWord word distance

spellcheck :: [Text] -> Int -> [Text] -> [SuggestedMatch]
spellcheck dictionary threshold =
    concatMap (spellcheckWord dictionary threshold)
