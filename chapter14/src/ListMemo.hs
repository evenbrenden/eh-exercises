{-# OPTIONS_GHC -fno-full-laziness #-}

module ListMemo where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Types

editDistance :: Text -> Text -> Int
editDistance stringA stringB = lookupEditDistance 0 0
  where
    distances =
        map (\idxA -> map (getEditDistance idxA) [0 .. bLen]) [0 .. aLen]
    lookupEditDistance idxA idxB = distances !! idxA !! idxB
    aLen = T.length stringA
    bLen = T.length stringB
    getEditDistance idxA idxB
        | idxA == aLen
        = bLen - idxB
        | idxB == bLen
        = aLen - idxA
        | T.index stringA idxA == T.index stringB idxB
        = lookupEditDistance (idxA + 1) (idxB + 1)
        | otherwise
        = let deleteCost = lookupEditDistance (idxA + 1) idxB
              insertCost = lookupEditDistance idxA (idxB + 1)
              swapCost   = lookupEditDistance (idxA + 1) (idxB + 1)
          in  1 + minimum [deleteCost, insertCost, swapCost]

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
