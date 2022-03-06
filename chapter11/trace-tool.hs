{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module CallTrace where

import           Text.Printf

data TraceData = forall a . Show a => TraceData a

instance Show TraceData where
    show (TraceData a) = show a

newtype Trace = Trace TraceData

emptyTrace :: Trace
emptyTrace = Trace $ TraceData $ mempty @String

traceCall :: (Show a, Show b) => String -> (a -> (Trace, b)) -> a -> (Trace, b)
traceCall s abt a =
    let (t, b) = abt a
        s'     = showTrace t
        t'     = Trace $ TraceData $ s <> show a <> " => " <> show b <> s'
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
