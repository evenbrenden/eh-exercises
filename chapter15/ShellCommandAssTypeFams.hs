{-# LANGUAGE TypeFamilies #-}

module ShellCommandAssTypeFams where

import Data.Kind
import System.Process (readProcess)

class ShellCommand cmd where
    type ShellOutput cmd :: Type
    runCmd ::
        Monad m =>
        cmd ->
        (String -> [String] -> m String) ->
        m (ShellOutput cmd)

newtype ListDirectory = ListDirectory {listDirectoryName :: FilePath}

instance ShellCommand ListDirectory where
    type ShellOutput ListDirectory = [FilePath]
    runCmd (ListDirectory dir) run =
        lines <$> run "ls" ["-1", dir]

data Grep = Grep {grepMatch :: String, grepFiles :: [String]}

data GrepMatch = GrepMatch
    { grepMatchingFileName :: FilePath,
      grepMatchingLineNumber :: Int,
      grepMatchingLineContents :: String
    }
    deriving (Eq, Show)

parseGrepResponse :: [String] -> [GrepMatch]
parseGrepResponse = map parseLine
    where
        parseLine responseLine =
            let (fileName, rest) = span (/= ':') responseLine
                (matchNumber, rest') = span (/= ':') $ tail rest
                contents = tail rest'
             in GrepMatch fileName (read matchNumber) contents

instance ShellCommand Grep where
    type ShellOutput Grep = [GrepMatch]
    runCmd (Grep match grepFiles) run =
        parseGrepResponse . fixResponses . lines <$> run "grep" grepArgs
        where
            grepArgs = "-n" : match : grepFiles
            fixResponses :: [String] -> [String]
            fixResponses responseLines =
                case grepFiles of
                    [fname] ->
                        (\l -> fname <> ":" <> l) <$> responseLines
                    _ ->
                        responseLines

data Pipe a b = Pipe a (ShellOutput a -> b)

instance (ShellCommand a, ShellCommand b) => ShellCommand (Pipe a b) where
    type ShellOutput (Pipe a b) = ShellOutput b
    runCmd (Pipe a mkB) run = do
        result <- runCmd a run
        runCmd (mkB result) run

grepFilesInDirectory :: String -> FilePath -> Pipe ListDirectory Grep
grepFilesInDirectory match dir =
    Pipe (ListDirectory dir) $ Grep match . map (\fname -> dir <> "/" <> fname)
