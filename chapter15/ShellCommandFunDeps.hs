{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module ShellCommandFunDeps where

import System.Process (readProcess)

class ShellCommand cmd cmdOutput | cmd -> cmdOutput where
    runCmd ::
        Monad m =>
        cmd ->
        (String -> [String] -> m String) ->
        m cmdOutput

newtype ListDirectory = ListDirectory {listDirectoryName :: FilePath}

instance ShellCommand ListDirectory [FilePath] where
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

instance ShellCommand Grep [GrepMatch] where
    runCmd (Grep match grepFiles) run =
        parseGrepResponse . fixResponses . lines <$> run "grep" ("-n" : match : grepFiles)
        where
            fixResponses :: [String] -> [String]
            fixResponses responseLines =
                case grepFiles of
                    [fname] -> (\l -> fname <> ":" <> l) <$> responseLines
                    _ -> responseLines

data Pipe a r b r' = Pipe a (r -> b)

-- IDK
-- instance (ShellCommand a, ShellCommand b) => ShellCommand (Pipe a b) where
--     type ShellOutput (Pipe a b) = ShellOutput b
--     runCmd (Pipe a mkB) run = do
--         result <- runCmd a run
--         runCmd (mkB result) run

grepFilesInDirectory ::
    String ->
    FilePath ->
    Pipe ListDirectory [FilePath] Grep [GrepMatch]
grepFilesInDirectory match dir =
    Pipe (ListDirectory dir) $ Grep match . map (\fname -> dir <> "/" <> fname)

runShellCommand :: ShellCommand cmd r => cmd -> IO r
runShellCommand cmd = runCmd cmd (\cmdName args -> readProcess cmdName args "")
