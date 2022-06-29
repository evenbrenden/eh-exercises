-- Remembering Common Typos

{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-full-laziness #-}

module Remember where

import           Control.Monad.ST
import           Data.Foldable                  ( for_
                                                , minimumBy
                                                )
import           Data.Hashable                  ( hash )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.STRef
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Unsafe              as TU
import           Data.Traversable
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

newtype Corrections s = Corrections (STRef s (Map Text SuggestedMatch))

newCorrections :: ST s (Corrections s)
newCorrections = Corrections <$> newSTRef Map.empty

readCorrections :: Corrections s -> Text -> ST s (Maybe SuggestedMatch)
readCorrections (Corrections ref) word = Map.lookup word <$> readSTRef ref

updateCorrections :: Corrections s -> Text -> SuggestedMatch -> ST s ()
updateCorrections (Corrections ref) word correction =
    modifySTRef ref $ Map.insert word correction

bestSuggestion :: [SuggestedMatch] -> SuggestedMatch
bestSuggestion =
    minimumBy (\m1 m2 -> compare (matchDistance m1) (matchDistance m2))

spellcheck :: [Text] -> Int -> [Text] -> [SuggestedMatch]
spellcheck dictionary threshold words =
    let
        hashedDictionary = (,) <*> hash <$> dictionary
        {-# INLINE correct #-}
        correct word = any ((hash word ==) . snd) hashedDictionary
        {-# INLINE checkWord #-}
        checkWord corrections word = if correct word
            then return []
            else do
                maybeCorrection <- readCorrections corrections word
                case maybeCorrection of
                    Just correction -> return $ pure correction
                    Nothing         -> do
                        let checked = spellcheckWord dictionary threshold word
                            best    = bestSuggestion checked
                        updateCorrections corrections word best
                        return $ pure best
    in
        runST $ do
            corrections <- newCorrections
            checked     <- traverse (checkWord corrections) words
            return $ concat checked
