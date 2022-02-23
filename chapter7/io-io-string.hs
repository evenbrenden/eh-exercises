{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text                     as Text
import           System.IO.Unsafe

f :: () -> IO (IO Text.Text)
f _ = return <$> Text.pack <$> getLine

g :: IO (IO Text.Text)
g = f () >>= return

h :: IO Text.Text
h = unsafePerformIO $ f ()
