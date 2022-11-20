{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module ShellCommandGADTs where

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
