{-# LANGUAGE ExistentialQuantification #-}

module CallTrace where

import           Text.Printf

data TraceData = forall a . Show a => TraceData a

instance Show TraceData where
    show (TraceData a) = show a

newtype Trace = Trace [TraceData]

emptyTrace :: Trace
emptyTrace = Trace []

traceCall :: (Show a, Show b) => String -> (a -> (Trace, b)) -> a -> (Trace, b)
traceCall s f a =
    let (Trace t, b) = f a
        s'           = s <> " " <> show a <> " => " <> show b
        t'           = Trace $ (TraceData s' : t)
    in  (t', b)

showTrace :: Trace -> String
showTrace (Trace a) =
    let
        sd = "stack depth: " <> show (length a) <> "\n"
        go n [] = ""
        go n (a : as) =
            (take n $ repeat '\t') <> (read . show) a <> "\n" <> go (n + 1) as
    in
        sd <> go 0 a

factor :: Int -> (Trace, [Int])
factor n = traceCall "factor" factor' (n, 2)
  where
    factor' :: (Int, Int) -> (Trace, [Int])
    factor' (num, curFact)
        | num == 1
        = (emptyTrace, [])
        | (num `mod` curFact) == 0
        = let
              nextNumber = num `div` curFact
              message    = "consFactor " <> show curFact
              (trace, results) =
                  traceCall message factor' (nextNumber, curFact)
          in
              (trace, curFact : results)
        | otherwise
        = let nextFactor = curFact + 1
          in  traceCall "skipFactor" factor' (num, nextFactor)

verboseFactor :: Int -> IO ()
verboseFactor n = do
    let (trace, factors) = factor n
    putStrLn "factors: "
    print factors
    putStrLn "trace: "
    putStrLn (showTrace trace)
