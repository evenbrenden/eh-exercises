{-# LANGUAGE BangPatterns #-}

import           Control.Concurrent             ( threadDelay )
import           Data.IORef
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromMaybe )
import           Data.Time.Clock                ( diffUTCTime
                                                , getCurrentTime
                                                , nominalDiffTimeToSeconds
                                                )

data AppMetrics = AppMetrics
    { successCount :: Int
    , failureCount :: Int
    , callDuration :: Map.Map String Int
    }
    deriving (Eq, Show)

newtype Metrics = Metrics { appMetricsStore :: IORef AppMetrics }

newMetrics :: IO Metrics
newMetrics =
    let emptyAppMetrics = AppMetrics { successCount = 0
                                     , failureCount = 0
                                     , callDuration = Map.empty
                                     }
    in  Metrics <$> newIORef emptyAppMetrics

timePureFunction :: Metrics -> String -> a -> IO a
timePureFunction (Metrics metrics) actionName action = do
    startTime <- getCurrentTime
    let !result = action
    endTime <- getCurrentTime
    modifyIORef metrics $ \oldMetrics ->
        let oldDurationValue = fromMaybe 0
                $ Map.lookup actionName (callDuration oldMetrics)
            runDuration =
                floor . nominalDiffTimeToSeconds $ diffUTCTime endTime startTime
            newDurationValue = oldDurationValue + runDuration
        in  oldMetrics
                { callDuration = Map.insert actionName newDurationValue
                                     $ callDuration oldMetrics
                }
    pure result

fibs = 0 : 1 : nextFibs fibs (tail fibs)
    where nextFibs (a : as) (b : bs) = (a + b) : nextFibs as bs

-- The limitation is that once the value is evaluated, we can't evaluate it
-- again. So running example a second time won't time it at all. The user would
-- have to ensure that the value is not evaluated before calling
-- timePureFunction. AFAIK there is no way to force reevaluation.
example = do
    metrics <- newMetrics
    let value = last $ take 500000 $ fibs
    timePureFunction metrics "fibs" value
    metrics' <- readIORef (appMetricsStore metrics)
    print metrics'
