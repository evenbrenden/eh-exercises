{-# OPTIONS_GHC -fno-full-laziness #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad                  ( when )
import qualified Data.Text                     as T
import qualified ListMemo
import qualified Naive
import qualified ST
import qualified System.Environment            as Env
import           Types

main :: IO ()
main = do
    [dictPath, checkPath, verbosity, algo] <- Env.getArgs
    dict  <- T.lines . T.pack <$> readFile dictPath
    check <- T.lines . T.pack <$> readFile checkPath
    let threshold = 3
    let result
            | algo == "listmemo" = ListMemo.spellcheck dict threshold check
            | algo == "naive"    = Naive.spellcheck dict threshold check
            | algo == "st"       = ST.spellcheck dict threshold check
            | otherwise          = fail "unknown algo"
    when (verbosity == "verbose") $ print (showSuggestedMatch <$> result)
    print $ length result
