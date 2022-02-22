{-# LANGUAGE TypeApplications #-}

prog :: IO ()
prog = do
    input <- getLine
    let nums = read @Int <$> words input
    putStrLn $ show $ sum nums
    return ()
