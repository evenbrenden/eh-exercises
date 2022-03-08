{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FilePack where

import           Data.Bits                      ( (.&.)
                                                , (.|.)
                                                , shift
                                                )
import qualified Data.ByteString               as BS
import           Data.ByteString.Char8          ( pack
                                                , unpack
                                                )
import qualified Data.Text                     as Text
import           Data.Text.Encoding             ( decodeUtf8
                                                , encodeUtf8
                                                )
import           Data.Word
import           System.Posix.Types

data FileData a = FileData
    { packedFileName        :: FilePath
    , packedFileSize        :: Word32
    , packedFilePermissions :: FileMode
    , packedFileData        :: a
    }
    deriving (Eq, Read, Show)

newtype FilePack = FilePack [Packable]

data Packable = forall a . Encode a => Packable
    { getPackable :: FileData a
    }

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

instance Encode a => Encode (FileData a) where
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

instance Encode FilePack where
    encode (FilePack p) = encode p

instance Encode Packable where
    encode (Packable p) = encode p

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

instance Encode FileMode where
    encode (CMode fMode) = encode fMode

instance Decode FileMode where
    decode = fmap CMode . decode

instance (Encode a, Encode b) => Encode (a, b) where
    encode (a, b) = encode $ encodeWithSize a <> encodeWithSize b

instance {-# OVERLAPPABLE #-} Encode a => Encode [a] where
    encode = encode . foldMap encodeWithSize

addFileDataToPack :: Encode a => FileData a -> FilePack -> FilePack
addFileDataToPack a (FilePack as) = FilePack $ (Packable a) : as

infixr 6 .:
(.:) :: (Encode a) => FileData a -> FilePack -> FilePack
(.:) = addFileDataToPack

emptyFilePack :: FilePack
emptyFilePack = FilePack []

testEncodeValue :: BS.ByteString
testEncodeValue =
    let a = FileData { packedFileName        = "a"
                     , packedFileSize        = 3
                     , packedFilePermissions = 0755
                     , packedFileData        = "foo" :: String
                     }
        b = FileData { packedFileName        = "b"
                     , packedFileSize        = 10
                     , packedFilePermissions = 0644
                     , packedFileData        = ["hello", "world"] :: [Text.Text]
                     }
        c = FileData { packedFileName        = "c"
                     , packedFileSize        = 8
                     , packedFilePermissions = 0644
                     , packedFileData        = (0, "zero") :: (Word32, String)
                     }
    in  encode $ a .: b .: c .: emptyFilePack

-- IDK, I think this is pretty ergonomic and easy?
nestedFilePack :: BS.ByteString
nestedFilePack =
    let a = FileData { packedFileName        = "a"
                     , packedFileSize        = 3
                     , packedFilePermissions = 0755
                     , packedFileData        = "foo" :: String
                     }
        b = FileData { packedFileName        = "b"
                     , packedFileSize        = 10
                     , packedFilePermissions = 0644
                     , packedFileData        = a
                     }
        c = FileData { packedFileName        = "c"
                     , packedFileSize        = 8
                     , packedFilePermissions = 0644
                     , packedFileData        = b
                     }
    in  encode $ a .: b .: c .: emptyFilePack
