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

timeFunction :: Metrics -> String -> IO a -> IO a
timeFunction (Metrics metrics) actionName action = do
    startTime <- getCurrentTime
    result    <- action
    endTime   <- getCurrentTime
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

example = do
    metrics <- newMetrics
    timeFunction metrics "wait" (threadDelay 1000000)
    metrics' <- readIORef (appMetricsStore metrics)
    print metrics'
