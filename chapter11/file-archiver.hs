{-# LANGUAGE OverloadedStrings #-}

module FilePack where

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Base64        as B64
import qualified Data.ByteString.Char8         as BC
import qualified Data.Text                     as Text
import           Data.Word
import           System.Posix.Types
import           Text.Read

data FileContents
    = StringFileContents String
    | TextFileContents Text.Text
    | ByteStringFileContents BS.ByteString
    deriving (Eq, Read, Show)

data FileData = FileData
    { fileName        :: FilePath
    , fileSize        :: Word32
    , filePermissions :: FileMode
    , fileData        :: FileContents
    }
    deriving (Eq, Read, Show)

newtype FilePack = FilePack {getPackedFiles :: [FileData]} deriving (Eq, Read, Show)

packFiles :: FilePack -> BS.ByteString
packFiles filePack = B64.encode . BC.pack . show $ filePack

unpackFiles :: BS.ByteString -> Either String FilePack
unpackFiles serializedData =
    B64.decode serializedData >>= readEither . BC.unpack

sampleFilePack :: FilePack
sampleFilePack =
    FilePack
        $ [ FileData "stringFile" 0 0 $ StringFileContents "hello string"
          , FileData "textFile" 0 0 $ TextFileContents "hello text"
          , FileData "binaryFile" 0 0
              $ ByteStringFileContents "hello bytestring"
          ]

testPackFile :: BS.ByteString
testPackFile = packFiles sampleFilePack

testUnpackFile :: Either String FilePack
testUnpackFile = unpackFiles testPackFile

testRoundTrip :: FilePack -> Bool
testRoundTrip pack = (Right pack) == (unpackFiles $ packFiles pack)
