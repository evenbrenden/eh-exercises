{-# LANGUAGE RecordWildCards #-}

module Types where

import           Data.Text                      ( Text )
import           Text.Printf

data SuggestedMatch = SuggestedMatch
    { matchWord         :: Text
    , matchSearchedWord :: Text
    , matchDistance     :: Int
    }
    deriving (Eq, Show)

showSuggestedMatch :: SuggestedMatch -> String
showSuggestedMatch SuggestedMatch {..} =
    printf "%s -> %s: %d" matchWord matchSearchedWord matchDistance
