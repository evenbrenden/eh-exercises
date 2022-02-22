module HCat where

import qualified Control.Exception             as Exception
import qualified Data.ByteString               as BS
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as TextIO
import qualified System.Environment            as Env
import           System.IO
import qualified System.IO.Error               as IOError
import           System.Info
import           System.Process                as Process

runHCat :: IO ()
runHCat =
    handleIOError
        $   handleArgs
        >>= eitherToErr
        >>= TextIO.readFile
        >>= \contents -> getTerminalSize >>= \termSize ->
                let pages = paginate termSize contents in showPages pages
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

getTerminalSize :: IO ScreenDimensions
getTerminalSize = case System.Info.os of
    "darwin" -> tputScreenDimensions
    "linux"  -> tputScreenDimensions
    _other   -> pure $ ScreenDimensions 25 80
  where
    tputScreenDimensions :: IO ScreenDimensions
    tputScreenDimensions =
        Process.readProcess "tput" ["lines"] "" >>= \lines ->
            Process.readProcess "tput" ["cols"] "" >>= \cols ->
                let lines' = read $ init lines
                    cols'  = read $ init cols
                in  return $ ScreenDimensions lines' cols'

data ScreenDimensions = ScreenDimensions
    { screenRows    :: Int
    , screenColumns :: Int
    }
    deriving Show

paginate :: ScreenDimensions -> Text.Text -> [Text.Text]
paginate (ScreenDimensions rows cols) text =
    let unwrappedLines = Text.lines text
        wrappedLines   = concatMap (wordWrap cols) unwrappedLines
        pageLines      = groupsOf rows wrappedLines
    in  map Text.unlines pageLines

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
showPages [] = return ()
showPages (page : pages) =
    clearScreen >> TextIO.putStrLn page >> getContinue >>= \case
        Continue -> showPages pages
        Cancel   -> return ()

clearScreen :: IO ()
clearScreen = BS.putStr "\^[[1J\^[[1;1H"

getContinue :: IO ContinueCancel
getContinue =
    hSetBuffering stdin NoBuffering
        >>  hSetEcho stdin False
        >>  hGetChar stdin
        >>= \case
                ' ' -> return Continue
                'q' -> return Cancel
                _   -> getContinue

data ContinueCancel = Continue | Cancel deriving (Eq, Show)
