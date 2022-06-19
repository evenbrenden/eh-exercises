{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Naive
import           Types

main :: IO ()
main =
    print $ showSuggestedMatch <$> spellcheck ["accept", "except"] 4 ["eksept"]
