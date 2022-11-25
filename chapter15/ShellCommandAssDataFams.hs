{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module ShellCommandAssDataFams where

import Data.Kind
import System.Process (readProcess)

class ShellCommand cmd where
    data ShellOutput cmd :: Type
    runCmd ::
        Monad m =>
        cmd ->
        (String -> [String] -> m String) ->
        m (ShellOutput cmd)

newtype ListDirectory = ListDirectory {listDirectoryName :: FilePath}

instance ShellCommand ListDirectory where
    data ShellOutput ListDirectory = DirectoryListing
        { containingDirectory :: FilePath,
          filenamesInListing :: [FilePath]
        }
        deriving (Show, Eq)
    runCmd (ListDirectory dir) run =
        DirectoryListing dir . lines <$> run "ls" ["-1", dir]

directoryListingWithParent :: ShellOutput ListDirectory -> [FilePath]
directoryListingWithParent DirectoryListing {..} =
    map fixPath filenamesInListing
    where
        fixPath fname =
            containingDirectory <> "/" <> fname

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
    newtype ShellOutput Grep = ListOfGrepMatches {getListOfGrepMatches :: [GrepMatch]}
    runCmd (Grep match grepFiles) run =
        ListOfGrepMatches . parseGrepResponse . fixResponses . lines <$> run "grep" grepArgs
        where
            grepArgs = "-n" : match : grepFiles
            fixResponses :: [String] -> [String]
            fixResponses responseLines =
                case grepFiles of
                    [fname] -> (\l -> fname <> ":" <> l) <$> responseLines
                    _ -> responseLines

-- A Complete Data Family Based ShellCommand PASS
data Pipe a b = Pipe a (ShellOutput a -> b)

instance (ShellCommand a, ShellCommand b) => ShellCommand (Pipe a b) where
    newtype ShellOutput (Pipe a b) = ShellOutput b
    runCmd (Pipe a mkB) run = do
        sa <- runCmd a run
        sb <- runCmd (mkB sa) run
        pure $ ShellOutput (mkB sa) -- What about sb

grepFilesInDirectory :: String -> FilePath -> Pipe ListDirectory Grep
grepFilesInDirectory match dir =
    let grep listDir = Grep match (filenamesInListing listDir)
     in Pipe (ListDirectory dir) grep
