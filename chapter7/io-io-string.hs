import           System.IO.Unsafe

f :: () -> IO (IO String)
f _ = return <$> getLine

g :: IO (IO String)
g = f () >>= return

h :: IO String
h = unsafePerformIO $ f ()
