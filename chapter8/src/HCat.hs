module HCat where

import qualified Control.Exception             as Exception
import qualified Data.ByteString               as BS
import qualified Data.Char                     as DC
import           Data.Maybe
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as TextIO
import qualified Data.Time.Clock               as Clock
import qualified Data.Time.Clock.POSIX         as PosixClock
import qualified Data.Time.Format              as TimeFormat
import qualified System.Directory              as Directory
import qualified System.Environment            as Env
import           System.IO
import qualified System.IO.Error               as IOError
import           System.Info
import           System.Process                as Process
import qualified Text.Printf                   as Printf

runHCat :: IO ()
runHCat = handleIOError $ do
    targetFilePath <- do
        args <- handleArgs
        eitherToErr args
    contents <- do
        handle <- openFile targetFilePath ReadMode
        TextIO.hGetContents handle
    termSize <- do
        size <- getTerminalSize
        eitherToErr size
    hSetBuffering stdout NoBuffering
    finfo <- fileInfo targetFilePath
    let pages = paginate termSize finfo contents
    showPages pages
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
    if isJust tput
        then case System.Info.os of
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
    noNewLine s = if null s then s else (head . lines) s
    isNumber s = not (null s) && all DC.isDigit s

data ScreenDimensions = ScreenDimensions
    { screenRows    :: Int
    , screenColumns :: Int
    }
    deriving Show

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
    padTo lineCOunt rowsToPad = take lineCOunt $ rowsToPad <> repeat ""

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

showPages :: [Text.Text] -> IO ()
showPages []             = return ()
showPages (page : pages) = do
    clearScreen
    TextIO.putStrLn page
    continuation <- getContinue
    case continuation of
        Continue -> showPages pages
        Cancel   -> return ()

clearScreen :: IO ()
clearScreen = BS.putStr "\^[[1J\^[[1;1H"

getContinue :: IO ContinueCancel
getContinue = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    input <- hGetChar stdin
    case input of
        ' ' -> return Continue
        'q' -> return Cancel
        _   -> getContinue

data ContinueCancel = Continue | Cancel deriving (Eq, Show)

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
