{-# OPTIONS_GHC -fno-full-laziness #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad                  ( when )
import qualified Data.Text                     as T
import qualified ListMemo
import qualified Micro
import qualified Naive
import qualified Correct
import qualified STRef
import qualified STVec
import qualified System.Environment            as Env
import           Types

-- Remembering Common Typos PASS
-- Spellchecking In HCat PASS

main :: IO ()
main = do
    [dictPath, checkPath, verbosity, algo] <- Env.getArgs
    dict  <- T.lines . T.pack <$> readFile dictPath
    check <- T.lines . T.pack <$> readFile checkPath
    let threshold = 3
    let result
            | algo == "listmemo" = ListMemo.spellcheck dict threshold check
            | algo == "naive"    = Naive.spellcheck dict threshold check
            | algo == "stref"    = STRef.spellcheck dict threshold check
            | algo == "stvec"    = STVec.spellcheck dict threshold check
            | algo == "micro"    = Micro.spellcheck dict threshold check
            | algo == "correct"    = Correct.spellcheck dict threshold check
            | otherwise          = fail "unknown algo"
    when (verbosity == "verbose") $ print (showSuggestedMatch <$> result)
    print $ length result
