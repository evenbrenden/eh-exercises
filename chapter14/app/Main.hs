{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad                  ( when )
import qualified Data.Text                     as T
import qualified Naive
import qualified System.Environment            as Env
import           Types

main :: IO ()
main = do
    [dictPath, checkPath, verbosity, algo] <- Env.getArgs
    dict  <- T.lines . T.pack <$> readFile dictPath
    check <- T.lines . T.pack <$> readFile checkPath
    let threshold = 3
    let result
            | algo == "naive"
            = showSuggestedMatch <$> Naive.spellcheck dict threshold check
            | otherwise
            = fail "unknown algo"
    when (verbosity == "verbose") $ print result
    print $ length result
