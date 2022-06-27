{-# OPTIONS_GHC -fno-full-laziness #-}

module STRef where

import           Control.Monad
import           Control.Monad.ST
import           Data.Foldable
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.STRef
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Types

newtype MemoCache s = MemoCache (STRef s (Map (Text, Text) Int))

readCache :: MemoCache s -> Text -> Text -> ST s (Maybe Int)
readCache (MemoCache ref) stringA stringB =
    Map.lookup (stringA, stringB) <$> readSTRef ref

updateCache :: MemoCache s -> Text -> Text -> Int -> ST s ()
updateCache (MemoCache ref) stringA stringB distance =
    modifySTRef ref $ Map.insert (stringA, stringB) distance

newCache :: ST s (MemoCache s)
newCache = MemoCache <$> newSTRef Map.empty

editDistance :: MemoCache s -> Text -> Text -> ST s Int
editDistance cache = memoizedEditDistance
  where
    memoizedEditDistance stringA stringB = do
        result <- readCache cache stringA stringB
        case result of
            Just distance -> pure distance
            Nothing       -> do
                newDistance <- findDistance stringA stringB
                updateCache cache stringA stringB newDistance
                pure newDistance

    findDistance stringA stringB
        | T.null stringA = pure $ T.length stringB
        | T.null stringB = pure $ T.length stringA
        | T.head stringA == T.head stringB = memoizedEditDistance restOfA
                                                                  restOfB
        | otherwise = do
            deleteCost <- memoizedEditDistance restOfA stringB
            insertCost <- memoizedEditDistance restOfA stringB
            swapCost   <- memoizedEditDistance restOfA stringB
            pure $ 1 + minimum [insertCost, deleteCost, swapCost]
      where
        restOfA = T.tail stringA
        restOfB = T.tail stringB

spellcheckWord :: MemoCache s -> [Text] -> Int -> Text -> ST s [SuggestedMatch]
spellcheckWord cache dictionary threshold word = foldM getSuggestions
                                                       []
                                                       dictionary
  where
    getSuggestions suggestions dictWord = do
        -- No habla cacheSuffixDistances
        distance <- editDistance cache dictWord word
        let suggestion = SuggestedMatch dictWord word distance
        if distance > 0 && distance <= threshold
            then pure (suggestion : suggestions)
            else pure suggestions

spellcheck :: [Text] -> Int -> [Text] -> [SuggestedMatch]
spellcheck dictionary threshold words = runST $ do
    cache <- newCache
    concat <$> traverse (spellcheckWord cache dictionary threshold) words
