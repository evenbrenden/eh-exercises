{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

module CallTrace where

import           Text.Printf

data TraceData = forall a . Show a => TraceData a

instance Show TraceData where
    show (TraceData a) = show a

newtype Trace = Trace [TraceData]

data TraceInfo = TraceInfo
    { fun :: String
    , arg :: String
    , res :: String
    }

instance Show TraceInfo where
    show TraceInfo {..} = fun <> " " <> arg <> " => " <> res

emptyTrace :: Trace
emptyTrace = Trace []

consTraceData :: TraceData -> Trace -> Trace
consTraceData (TraceData t) (Trace ts) = Trace (TraceData t : ts)

makeTraceData :: String -> String -> String -> TraceData
makeTraceData f a r = TraceData $ TraceInfo f a r

traceCall :: (Show a, Show b) => String -> (a -> (Trace, b)) -> a -> (Trace, b)
traceCall s f a =
    let (t, r) = f a
        td     = makeTraceData s (show a) (show r)
        t'     = consTraceData td t
    in  (t', r)

tabs :: Int -> String
tabs n = take n $ repeat '\t'

showTrace :: Trace -> String
showTrace (Trace ts) =
    let sd     = length ts
        header = "stack depth: " <> show sd <> "\n"
        folder t (s, n) = (tabs n <> show t <> "\n" <> s, n - 1)
        folded = foldr folder (mempty, sd - 1) ts
    in  header <> fst folded

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
