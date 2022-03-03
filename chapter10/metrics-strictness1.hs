{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Exception              ( IOException
                                                , handle
                                                )
import           Control.Monad                  ( when )
import           Data.Foldable                  ( for_ )
import           Data.IORef
import           Data.List                      ( isSuffixOf )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Set                      as Set
                                                ( empty
                                                , insert
                                                , member
                                                )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as TextIO
import           Data.Time.Clock                ( diffUTCTime
                                                , getCurrentTime
                                                , nominalDiffTimeToSeconds
                                                )
import           System.Directory               ( canonicalizePath
                                                , doesDirectoryExist
                                                , doesFileExist
                                                , listDirectory
                                                )
import           System.Environment             ( getArgs )
import           Text.Printf

data MetricsStore = MetricsStore
    { successCount :: Int
    , failureCount :: Int
    , callDuration :: (Map.Map String Int)
    }
    deriving (Eq, Show)

newtype Metrics = Metrics { appMetricsStore :: IORef MetricsStore }

displayMetrics :: Metrics -> IO ()
displayMetrics metrics = do
    metrics' <- readIORef (appMetricsStore metrics)
    putStrLn $ "successCount = " <> show (successCount metrics')
    putStrLn $ "failureCount = " <> show (failureCount metrics')
    putStrLn $ "callDuration = " <> show (callDuration metrics')

newMetrics :: IO Metrics
newMetrics =
    let emptyMetricsStore = MetricsStore { successCount = 0
                                         , failureCount = 0
                                         , callDuration = Map.empty
                                         }
    in  Metrics <$> newIORef emptyMetricsStore

tickSuccess :: Metrics -> IO ()
tickSuccess (Metrics metricsRef) =
    let update m = m { successCount = 1 + successCount m }
    in  modifyIORef' metricsRef update

tickFailure :: Metrics -> IO ()
tickFailure (Metrics metricsRef) =
    let update m = m { failureCount = 1 + failureCount m }
    in  modifyIORef' metricsRef update

timeFunction :: Metrics -> String -> IO a -> IO a
timeFunction (Metrics metrics) actionName action = do
    startTime <- getCurrentTime
    result    <- action
    endTime   <- getCurrentTime
    modifyIORef' metrics $ \oldMetrics ->
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

data FileType
    = FileTypeDirectory
    | FileTypeRegularFile
    | FileTypeOther

classifyFile :: FilePath -> IO FileType
classifyFile fname = do
    isDirectory <- doesDirectoryExist fname
    isFile      <- doesFileExist fname
    pure $ case (isDirectory, isFile) of
        (True , False) -> FileTypeDirectory
        (False, True ) -> FileTypeRegularFile
        _otherwise     -> FileTypeOther

dropSuffix :: String -> String -> String
dropSuffix suffix s | suffix `isSuffixOf` s = take (length s - length suffix) s
                    | otherwise             = s

traverseDirectory :: Metrics -> FilePath -> (FilePath -> IO ()) -> IO ()
traverseDirectory metrics rootPath action = do
    seenRef <- newIORef Set.empty
    let haveSeenDirectory canonicalPath =
            Set.member canonicalPath <$> readIORef seenRef
        addDirectoryToSeen canonicalPath =
            modifyIORef' seenRef $ Set.insert canonicalPath
        traverseSubdirectory subdirPath =
            timeFunction metrics "traverseSubdirectory" $ do
                contents <- listDirectory subdirPath
                for_ contents $ \file' ->
                    handle @IOException
                            (\ex -> putStrLn (show ex) >> tickFailure metrics)
                        $ do
                              let file = subdirPath <> "/" <> file'
                              canonicalPath  <- canonicalizePath file
                              classification <- classifyFile canonicalPath
                              result         <- case classification of
                                  FileTypeOther       -> pure ()
                                  FileTypeRegularFile -> action file
                                  FileTypeDirectory   -> do
                                      alreadyProcessed <- haveSeenDirectory file
                                      when (not alreadyProcessed) $ do
                                          addDirectoryToSeen file
                                          traverseSubdirectory file
                              tickSuccess metrics
                              pure result
    traverseSubdirectory (dropSuffix "/" rootPath)

directorySummaryWithMetrics :: FilePath -> IO ()
directorySummaryWithMetrics root = do
    metrics      <- newMetrics
    histogramRef <- newIORef (Map.empty :: Map.Map Char Int)
    traverseDirectory metrics root $ \file -> do
        putStrLn $ file <> ":"
        contents <- timeFunction metrics "TextIO.readFile"
            $ TextIO.readFile file
        timeFunction metrics "wordcount"
            $ let wordCount = length $ Text.words contents
              in  putStrLn $ " word count: " <> show wordCount
        timeFunction metrics "histogram" $ do
            oldHistogram <- readIORef histogramRef
            let addCharToHistogram histogram letter =
                    Map.insertWith (+) letter 1 histogram
                newHistogram =
                    Text.foldl' addCharToHistogram oldHistogram contents
            modifyIORef' histogramRef (const $ newHistogram)
    histogram <- readIORef histogramRef
    putStrLn "Histogram Data:"
    for_ (Map.toList histogram)
        $ \(letter, count) -> putStrLn $ printf " %c: %d" letter count
    displayMetrics metrics

main = getArgs >>= directorySummaryWithMetrics . head
