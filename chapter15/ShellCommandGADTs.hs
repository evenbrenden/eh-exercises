{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ShellCommandGADTs where

import qualified Data.Set as S
import System.FilePath.Posix ((</>))
import System.IO.Error
import System.Process (readProcess)

newtype ProgName = ProgName {getProgName :: FilePath}
newtype ProgArgs = ProgArgs {getProgArgs :: [String]}

data ShellCmd a b where
    RunCommand ::
        ProgName ->
        (a -> ProgArgs) ->
        (a -> String -> b) ->
        ShellCmd a b
    Pipe :: ShellCmd a b -> ShellCmd b c -> ShellCmd a c
    XArgs :: ShellCmd a b -> ShellCmd [a] [b]
    MapOut :: (b -> c) -> ShellCmd b c

data GrepMatch = GrepMatch
    { grepMatchingFileName :: FilePath,
      grepMatchingLineNumber :: Int,
      grepMatchingLineContents :: String
    }
    deriving (Eq, Show)

grep :: String -> ShellCmd FilePath [GrepMatch]
grep matchGlob =
    RunCommand (ProgName "grep") makeArgs parseLines
    where
        makeArgs fileName = ProgArgs $ "-n" : matchGlob : [fileName]
        parseLines fileName = map (parseResponse fileName) . lines
        parseResponse fileName responseLine =
            let (matchNumber, contents) = span (/= ':') responseLine
             in GrepMatch fileName (read matchNumber) contents

listDirectory :: ShellCmd FilePath [FilePath]
listDirectory =
    RunCommand (ProgName "ls") makeArgs parseResponse
    where
        makeArgs filePath = ProgArgs ["-1", filePath]
        parseResponse filePath =
            map (filePath </>) . lines

runShellCmd :: ShellCmd a b -> a -> IO b
runShellCmd cmd input =
    case cmd of
        RunCommand (ProgName exeName) mkArgs parseOut ->
            parseOut input <$> catchIOError processOut (const $ pure "")
            where
                processOut = readProcess exeName (getProgArgs $ mkArgs input) ""
        Pipe inputCmd out -> runShellCmd inputCmd input >>= runShellCmd out
        XArgs inputCmd -> mapM (runShellCmd inputCmd) input
        MapOut mapF -> pure $ mapF input

-- Expanded Shell Commands PASS

wc :: ShellCmd FilePath Int
wc =
    RunCommand (ProgName "wc") makeArgs parseResponse
    where
        makeArgs filePath = ProgArgs [filePath]
        parseResponse _ = read @Int . head . words

dedup :: Ord a => [a] -> [a]
dedup = S.toList . S.fromList

-- ls -1 | xargs grep -n GLOB | sed s/:.*$//g | sort -u | xargs wc -l | head -n -1
-- countLinesInMatchingFiles :: String -> ShellCmd FilePath [(FilePath, Int)]
countLinesInMatchingFiles glob =
    let p1 = Pipe listDirectory (XArgs (grep glob))
        p2 = Pipe p1 (MapOut (dedup . fmap grepMatchingFileName . concat))
        p3 = Pipe p2 (XArgs wc)
     in p3
