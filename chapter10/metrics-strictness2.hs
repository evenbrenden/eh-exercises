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
    { successCount :: IORef Int
    , failureCount :: IORef Int
    , callDuration :: IORef (Map.Map String Int)
    }

newtype Metrics = Metrics { appMetricsStore :: MetricsStore }

displayMetrics :: Metrics -> IO ()
displayMetrics metrics = do
    let metrics' = appMetricsStore metrics
    successCount' <- readIORef (successCount metrics')
    failureCount' <- readIORef (failureCount metrics')
    callDuration' <- readIORef (callDuration metrics')
    putStrLn $ "successCount = " <> show successCount'
    putStrLn $ "failureCount = " <> show failureCount'
    putStrLn $ "callDuration = " <> show callDuration'

newMetrics :: IO Metrics
newMetrics = do
    sZero  <- newIORef 0
    fZero  <- newIORef 0
    cEmpty <- newIORef Map.empty
    let emptyMetricsStore = MetricsStore { successCount = sZero
                                         , failureCount = fZero
                                         , callDuration = cEmpty
                                         }
    return $ Metrics emptyMetricsStore

tickSuccess :: Metrics -> IO ()
tickSuccess (Metrics metricsRef) =
    modifyIORef' (successCount metricsRef) $ (+ 1)

tickFailure :: Metrics -> IO ()
tickFailure (Metrics metricsRef) =
    modifyIORef' (failureCount metricsRef) $ (+ 1)

timeFunction :: Metrics -> String -> IO a -> IO a
timeFunction (Metrics metrics) actionName action = do
    startTime <- getCurrentTime
    result    <- action
    endTime   <- getCurrentTime
    modifyIORef' (callDuration metrics) $ \oldCallDuration ->
        let oldDurationValue =
                fromMaybe 0 $ Map.lookup actionName oldCallDuration
            runDuration =
                floor . nominalDiffTimeToSeconds $ diffUTCTime endTime startTime
            newDurationValue = oldDurationValue + runDuration
        in  Map.insert actionName newDurationValue oldCallDuration
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
