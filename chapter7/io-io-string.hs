{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import qualified Data.Text                     as Text

x :: IO (IO Text.Text)
x = return <$> Text.pack <$> getLine

y :: IO (IO Text.Text)
y = x >>= return

z :: IO Text.Text
z = join $ x
