import           System.IO.Unsafe

f :: IO (IO a) -> IO a
f = unsafePerformIO
