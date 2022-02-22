{-# LANGUAGE TypeApplications #-}

import           Data.Monoid

data Op = Add | Sub | Mul

-- Since number subtraction is not associative it does not form a semigroup
calc :: Op -> [Int] -> Int
calc Add ns       = getSum $ foldMap Sum ns
calc Sub []       = getSum $ mempty
calc Sub (n : ns) = getSum $ foldMap Sum $ n : (negate <$> ns)
calc Mul ns       = getProduct $ foldMap Product ns

prog :: Op -> IO ()
prog op = do
    input <- getLine
    let nums = read @Int <$> words input
    putStrLn $ show $ calc op nums
    return ()
