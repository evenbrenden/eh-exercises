module HCat where

import qualified Control.Exception             as Exception
import qualified Data.ByteString               as BS
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as TextIO
import qualified System.Environment            as Env
import qualified System.IO.Error               as IOError

handleArgs :: IO (Either String FilePath)
handleArgs = parseArgs <$> Env.getArgs
  where
    parseArgs argumentList = case argumentList of
        [fname] -> Right fname
        []      -> Left "no filename provided"
        _       -> Left "multiple files not supported"

runHCat :: IO ()
runHCat =
    handleIOError
        $   handleArgs
        >>= eitherToErr
        >>= TextIO.readFile
        >>= TextIO.putStrLn
  where
    handleIOError :: IO () -> IO ()
    handleIOError ioAction = Exception.catch ioAction handleErr
    handleErr :: IOError -> IO ()
    handleErr e = print "I ran into an error!" >> print e

eitherToErr :: Show a => Either a b -> IO b
eitherToErr (Right a) = return a
eitherToErr (Left  e) = Exception.throwIO . IOError.userError $ show e
