import           Control.Monad

f :: IO (IO a) -> IO a
f = join
