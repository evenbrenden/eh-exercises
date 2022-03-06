{-# LANGUAGE ExistentialQuantification #-}

module CallTrace where

import           Text.Printf

data TraceData = forall a . Show a => TraceData
    { trace :: a
    , depth :: Int
    }

instance Show TraceData where
    show (TraceData a _) = show a

newtype Trace = Trace TraceData

emptyTrace :: Trace
emptyTrace = Trace $ TraceData "done!" 0

stackDepth :: Trace -> Int
stackDepth (Trace t) = depth t

traceCall :: (Show a, Show b) => String -> (a -> (Trace, b)) -> a -> (Trace, b)
traceCall s f a =
    let (t, b) = f a
        d      = stackDepth t + 1
        s'     = showTrace t
        t'     = Trace $ TraceData
            (  s
            <> " "
            <> show a
            <> " => "
            <> show b
            <> " (depth is "
            <> show d
            <> ") -> "
            <> read s'
            )
            d
    in  (t', b)

showTrace :: Trace -> String
showTrace (Trace a) = show a

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
