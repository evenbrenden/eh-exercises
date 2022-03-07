{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FilePack where

import           Data.Bits                      ( (.&.)
                                                , (.|.)
                                                , shift
                                                )
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Base64        as B64
import qualified Data.ByteString.Char8         as BC
import           Data.ByteString.Char8          ( pack
                                                , unpack
                                                )
import           Data.ByteString.Char8          ( pack
                                                , unpack
                                                )
import qualified Data.Text                     as Text
import           Data.Text.Encoding             ( decodeUtf8
                                                , encodeUtf8
                                                )
import           Data.Word
import           System.Posix.Types
import           Text.Read

data FileContents
    = StringFileContents String
    | TextFileContents Text.Text
    | ByteStringFileContents BS.ByteString
    deriving (Eq, Read, Show)

data FileData = FileData
    { packedFileName        :: FilePath
    , packedFileSize        :: Word32
    , packedFilePermissions :: FileMode
    , packedFileData        :: FileContents
    }
    deriving (Eq, Read, Show)

newtype FilePack = FilePack {getPackedFiles :: [FileData]} deriving (Eq, Read, Show)

class Encode a where
    encode :: a -> BS.ByteString
    encode =
        BS.drop 4 . encodeWithSize
    encodeWithSize :: a -> BS.ByteString
    encodeWithSize a =
        let s = encode a
            l = fromIntegral $ BS.length s
        in word32ToByteString l <> s
    {- MINIMAL encode | encodeWithSize #-}

class Decode a where
    decode :: BS.ByteString -> Either String a

instance Encode BS.ByteString where
    encode = id

instance Decode BS.ByteString where
    decode = Right . id

instance Encode Text.Text where
    encode = encodeUtf8

instance Decode Text.Text where
    decode = Right . decodeUtf8

instance Encode String where
    encode = pack

instance Decode String where
    decode = Right . unpack

instance Encode Word32 where
    encode = word32ToByteString
    encodeWithSize w =
        let (a, b, c, d) = word32ToBytes w in BS.pack [4, 0, 0, 0, a, b, c, d]

instance Decode Word32 where
    decode = bytestringToWord32

instance Encode FileMode where
    encode (CMode fMode) = encode fMode

instance Decode FileMode where
    decode = fmap CMode . decode

instance Encode FileContents where
    encode (StringFileContents     x) = encode x
    encode (TextFileContents       x) = encode x
    encode (ByteStringFileContents x) = encode x

instance Encode FileData where
    encode FileData {..} =
        let
            encodedFileName        = encodeWithSize packedFileName
            encodedFileSize        = encodeWithSize packedFileSize
            encodedFilePermissions = encodeWithSize packedFilePermissions
            encodedFileData        = encodeWithSize packedFileData
            encodedData =
                encodedFileName
                    <> encodedFileSize
                    <> encodedFilePermissions
                    <> encodedFileData
        in
            encode encodedData

instance (Encode a, Encode b) => Encode (a,b) where
    encode (a, b) = encode $ encodeWithSize a <> encodeWithSize b

instance {-# OVERLAPPABLE #-} Encode a => Encode [a] where
    encode = encode . foldMap encodeWithSize

word32ToBytes :: Word32 -> (Word8, Word8, Word8, Word8)
word32ToBytes word =
    let a = fromIntegral $ 255 .&. word
        b = fromIntegral $ 255 .&. (shift word (-8))
        c = fromIntegral $ 255 .&. (shift word (-16))
        d = fromIntegral $ 255 .&. (shift word (-24))
    in  (a, b, c, d)

word32FromBytes :: (Word8, Word8, Word8, Word8) -> Word32
word32FromBytes (a, b, c, d) =
    let a' = fromIntegral a
        b' = shift (fromIntegral b) 8
        c' = shift (fromIntegral c) 16
        d' = shift (fromIntegral d) 24
    in  a' .|. b' .|. c' .|. d'

word32ToByteString :: Word32 -> BS.ByteString
word32ToByteString word =
    let (a, b, c, d) = word32ToBytes word in BS.pack [a, b, c, d]

bytestringToWord32 :: BS.ByteString -> Either String Word32
bytestringToWord32 bytestring = case BS.unpack bytestring of
    [a, b, c, d] -> Right $ word32FromBytes (a, b, c, d)
    _otherwise ->
        let l = show $ BS.length bytestring
        in  Left ("Expecting 4 bytes but got " <> l)

consWord32 :: Word32 -> BS.ByteString -> BS.ByteString
consWord32 word bytestring =
    let packedWord = word32ToByteString word in packedWord <> bytestring

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
