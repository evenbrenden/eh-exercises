module HCat where

import qualified Control.Exception             as Exception
import qualified Data.ByteString               as BS
import qualified Data.Char                     as DC
import qualified Data.Maybe                    as DM
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as TextIO
import qualified Data.Time.Clock               as Clock
import qualified Data.Time.Clock.POSIX         as PosixClock
import qualified Data.Time.Format              as TimeFormat
import qualified System.Directory              as Directory
import qualified System.Environment            as Env
import           System.IO
import qualified System.IO.Error               as IOError
import qualified System.Info                   as SI
import           System.Process                as Process
import qualified Text.Printf                   as Printf

-- Viewing Multiple Files PASS
-- Add A Help Screen PASS

runHCat :: IO ()
runHCat = handleIOError $ do
    targetFilePath <- do
        args <- handleArgs
        eitherToErr args
    contents <- do
        handle <- openFile targetFilePath ReadMode
        TextIO.hGetContents handle
    hSetBuffering stdout NoBuffering
    finfo <- fileInfo targetFilePath
    let file = File finfo contents
    pages <- resize file
    showPages pages 0 file
  where
    handleIOError :: IO () -> IO ()
    handleIOError ioAction = Exception.catch ioAction handleErr
    handleErr :: IOError -> IO ()
    handleErr e = print "I ran into an error!" >> print e

handleArgs :: IO (Either String FilePath)
handleArgs = parseArgs <$> Env.getArgs
  where
    parseArgs argumentList = case argumentList of
        [fname] -> Right fname
        []      -> Left "no filename provided"
        _       -> Left "multiple files not supported"

eitherToErr :: Show a => Either a b -> IO b
eitherToErr (Right a) = return a
eitherToErr (Left  e) = Exception.throwIO . IOError.userError $ show e

getTerminalSize :: IO (Either Text.Text ScreenDimensions)
getTerminalSize = do
    tput <- Directory.findExecutable "tput"
    if DM.isJust tput
        then case SI.os of
            "darwin" -> tputScreenDimensions
            "linux"  -> tputScreenDimensions
            _other   -> pure $ Right $ ScreenDimensions 25 80
        else pure $ Left "tput not found"
  where
    tputScreenDimensions :: IO (Either Text.Text ScreenDimensions)
    tputScreenDimensions = do
        lines <- noNewLine <$> Process.readProcess "tput" ["lines"] ""
        cols  <- noNewLine <$> Process.readProcess "tput" ["cols"] ""
        if isNumber lines && isNumber cols
            then
                let lines' = read lines
                    cols'  = read cols
                in  return $ Right $ ScreenDimensions lines' cols'
            else return $ Left "tput output is not a number"
    noNewLine s = case lines s of
        []       -> []
        (x : xs) -> x
    isNumber s = not (null s) && all DC.isDigit s

data ScreenDimensions = ScreenDimensions
    { screenRows    :: Int
    , screenColumns :: Int
    }
    deriving Show

-- There is a defect in this function where the first line of each page is
-- missing.
paginate :: ScreenDimensions -> FileInfo -> Text.Text -> [Text.Text]
paginate (ScreenDimensions rows cols) finfo text =
    let
        rows'        = rows - 1
        wrappedLines = concatMap (wordWrap cols) (Text.lines text)
        pages = map (Text.unlines . padTo rows') $ groupsOf rows' wrappedLines
        pageCount    = length pages
        statusLines =
            map (formatFileInfo finfo cols pageCount) [1 .. pageCount]
    in
        zipWith (<>) pages statusLines
  where
    padTo :: Int -> [Text.Text] -> [Text.Text]
    padTo lineCount rowsToPad = take lineCount $ rowsToPad <> repeat ""

groupsOf :: Int -> [a] -> [[a]]
groupsOf n []    = []
groupsOf n elems = let (hd, tl) = splitAt n elems in hd : groupsOf n tl

wordWrap :: Int -> Text.Text -> [Text.Text]
wordWrap lineLength lineText
    | Text.length lineText <= lineLength
    = [lineText]
    | otherwise
    = let (candidate, nextLines) = Text.splitAt lineLength lineText
          (firstLine, overflow) =
              softWrap candidate (Text.length candidate - 1)
      in  firstLine : wordWrap lineLength (overflow <> nextLines)
  where
    softWrap hardwrappedText textIndex
        | textIndex <= 0
        = (hardwrappedText, Text.empty)
        | Text.index hardwrappedText textIndex == ' '
        = let (wrappedLine, rest) = Text.splitAt textIndex hardwrappedText
          in  (wrappedLine, Text.tail rest)
        | otherwise
        = softWrap hardwrappedText (textIndex - 1)

-- We need the original file content since we lose the original line endings
-- when paginating. Alternatively, we could reload the original file from disk.
-- That would save memory, but we would risk loading a changed or missing file.
resize :: File -> IO [Text.Text]
resize File {..} = do
    termSize <- do
        size <- getTerminalSize
        eitherToErr size
    return $ paginate termSize info contents

data File = File
    { info     :: FileInfo
    , contents :: Text.Text
    }

showPages :: [Text.Text] -> Int -> File -> IO ()
showPages pages position file = do
    clearScreen
    let maybePage = DM.listToMaybe $ drop position pages
    case maybePage of
        Just page -> do
            TextIO.putStrLn page
            continuation <- getNextStep
            case continuation of
                Backward -> showPages pages (nonNegativePred position) file
                Forward  -> showPages pages (succ position) file
                Resize   -> do
                    resized <- resize file
                    showPages resized position file
                Cancel -> return ()
        Nothing -> return ()
    where nonNegativePred i = max 0 (pred i)

clearScreen :: IO ()
clearScreen = BS.putStr "\^[[1J\^[[1;1H"

getNextStep :: IO NextStep
getNextStep = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    input <- hGetChar stdin
    case input of
        'b' -> return Backward
        'f' -> return Forward
        'r' -> return Resize
        'q' -> return Cancel
        _   -> getNextStep

data NextStep = Backward | Forward | Resize | Cancel deriving (Eq, Show)

formatFileInfo :: FileInfo -> Int -> Int -> Int -> Text.Text
formatFileInfo FileInfo {..} maxWidth totalPages currentPage =
    let
        timestamp =
            TimeFormat.formatTime TimeFormat.defaultTimeLocale "%F %T" fileMTime
        permissionString =
            [ if fileReadable then 'r' else '-'
            , if fileWriteable then 'w' else '-'
            , if fileExecutable then 'x' else '-'
            ]
        statusLine = Text.pack $ Printf.printf
            "%s | permissions: %s | %d bytes | modified: %s | page: %d of %d"
            filePath
            permissionString
            fileSize
            timestamp
            currentPage
            totalPages
    in
        invertText (truncateStatus statusLine)
  where
    invertText inputStr =
        let reverseVideo = "\^[[7m"
            resetVideo   = "\^[[0m"
        in  reverseVideo <> inputStr <> resetVideo
    truncateStatus statusLine
        | maxWidth <= 3
        = ""
        | Text.length statusLine > maxWidth
        = Text.take (maxWidth - 3) statusLine <> "..."
        | otherwise
        = statusLine

fileInfo :: FilePath -> IO FileInfo
fileInfo filePath = do
    perms <- Directory.getPermissions filePath
    mtime <- Directory.getModificationTime filePath
    size  <- Text.length <$> TextIO.readFile filePath
    return FileInfo { filePath       = filePath
                    , fileSize       = size
                    , fileMTime      = mtime
                    , fileReadable   = Directory.readable perms
                    , fileWriteable  = Directory.writable perms
                    , fileExecutable = Directory.executable perms
                    }

data FileInfo = FileInfo
    { filePath       :: FilePath
    , fileSize       :: Int
    , fileMTime      :: Clock.UTCTime
    , fileReadable   :: Bool
    , fileWriteable  :: Bool
    , fileExecutable :: Bool
    }
    deriving Show
