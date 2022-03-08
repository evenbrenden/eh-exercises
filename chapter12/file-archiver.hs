{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module FileArchiver where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Fail
import           Data.Bits                      ( (.&.)
                                                , (.|.)
                                                , shift
                                                )
import qualified Data.ByteString               as BS
import           Data.ByteString.Char8          ( pack
                                                , unpack
                                                )
import qualified Data.ByteString.Char8         as ByteStringChar8
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
    deriving (Eq, Show)

class Encode a where
    encode :: a -> BS.ByteString
    encode = BS.drop 4 . encodeWithSize
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
    decode = Right

instance Encode Text.Text where
    encode = encodeUtf8

instance Decode Text.Text where
    decode = Right . decodeUtf8

instance Encode String where
    encode = pack

instance Decode String where
    decode = Right . unpack

instance Encode FileMode where
    encode (CMode fMode) = encode fMode
    encodeWithSize (CMode fMode) = encodeWithSize fMode

instance Decode FileMode where
    decode = fmap CMode . decode

word32ToBytes :: Word32 -> (Word8, Word8, Word8, Word8)
word32ToBytes word =
    let a = fromIntegral $ 255 .&. word
        b = fromIntegral $ 255 .&. shift word (-8)
        c = fromIntegral $ 255 .&. shift word (-16)
        d = fromIntegral $ 255 .&. shift word (-24)
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
    _ ->
        let l = show $ BS.length bytestring
        in  Left ("Expecting 4 bytes but got " <> l)

instance Encode Word32 where
    encode = word32ToByteString
    encodeWithSize w =
        let (a, b, c, d) = word32ToBytes w in BS.pack [4, 0, 0, 0, a, b, c, d]

instance Decode Word32 where
    decode = bytestringToWord32

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

instance (Encode a, Encode b) => Encode (a,b) where
    encode (a, b) = encode $ encodeWithSize a <> encodeWithSize b

instance {-# OVERLAPPABLE #-} Encode a => Encode [a] where
    encode = encode . foldMap encodeWithSize

data Packable = forall a . Encode a => Packable
    { getPackable :: FileData a
    }

instance Encode Packable where
    encode (Packable p) = encode p

newtype FilePack = FilePack [Packable]

instance Encode FilePack where
    encode (FilePack p) = encode p

naiveDecodeWord32 :: BS.ByteString -> Either String (Word32, BS.ByteString)
naiveDecodeWord32 inputString = do
    when (BS.length inputString < 4)
        $ Left "Error, not enough data to get the size of the next field"
    let (encodedSizePrefix, rest) = BS.splitAt 4 inputString
    sizePrefix <- bytestringToWord32 encodedSizePrefix
    when (sizePrefix /= 4) $ Left "the field size of a word should be 4"
    when (BS.length rest < fromIntegral sizePrefix)
        $ Left "Not enough data for the next field size"
    let (encodedWord, rest') = BS.splitAt 4 rest
    decodedWord <- decode encodedWord
    pure (decodedWord, rest')

naiveDecodedString :: BS.ByteString -> Either String (String, BS.ByteString)
naiveDecodedString inputString = do
    when (BS.length inputString < 4)
        $ Left "Error, not enough data to get the size of the next field"
    let (encodedSizePrefix, rest) = BS.splitAt 4 inputString
    sizePrefix <- bytestringToWord32 encodedSizePrefix
    when (BS.length rest < fromIntegral sizePrefix)
        $ Left "Not enough data for the next field size"
    let (encodedString, rest') = BS.splitAt 4 rest
    decodedString <- decode encodedString
    pure (decodedString, rest')

extractBytes
    :: Int -> BS.ByteString -> Either String (BS.ByteString, BS.ByteString)
extractBytes n byteString = do
    when (BS.length byteString < n)
        $  Left
        $  "Error, extract bytes needs at least "
        <> show n
        <> " bytes"
    pure $ BS.splitAt n byteString

nextSegmentSize :: BS.ByteString -> Either String (Word32, BS.ByteString)
nextSegmentSize byteString = do
    (nextSegmentStr, rest) <- extractBytes 4 byteString
    parsedSegmentSize      <- bytestringToWord32 nextSegmentStr
    pure (parsedSegmentSize, rest)

nextSegment :: BS.ByteString -> Either String (BS.ByteString, BS.ByteString)
nextSegment byteString = do
    (segmentSize, rest) <- nextSegmentSize byteString
    extractBytes (fromIntegral segmentSize) rest

newtype FilePackParser a = FilePackParser
    { runParser :: BS.ByteString -> Either String (a, BS.ByteString) }

instance Functor FilePackParser where
    fmap f parser = FilePackParser $ \input -> do
        (parsedValue, result) <- runParser parser input
        pure (f parsedValue, result)

instance Applicative FilePackParser where
    pure a = FilePackParser $ \s -> pure (a, s)
    f <*> s = FilePackParser $ \input -> do
        (f', initialRemainder) <- runParser f input
        (a , finalRemainder  ) <- runParser s initialRemainder
        pure (f' a, finalRemainder)

extractValue :: Decode a => FilePackParser a
extractValue = FilePackParser $ \input -> do
    when (BS.length input < 4)
        $ Left "Input has less than 4 bytes, we can't get a segment size"
    let (rawSegmentSize, rest) = BS.splitAt 4 input
    segmentSize <- fromIntegral <$> bytestringToWord32 rawSegmentSize
    when (BS.length rest < segmentSize)
        $  Left
        $  "not enough input to parse the next value"
        <> (show input)
    let (rawSegmentValue, rest') = BS.splitAt segmentSize rest
    case decode rawSegmentValue of
        Left  err -> Left err
        Right a   -> Right (a, rest')

execParser :: FilePackParser a -> BS.ByteString -> Either String a
execParser parser inputString = fst <$> runParser parser inputString

data SomeRecord = SomeRecord
    { recordNumber :: Word32
    , recordString :: String
    , recordTuple  :: (Word32, String)
    }
    deriving (Eq, Show)

exampleRecord :: SomeRecord
exampleRecord = SomeRecord 1 "two" (3, "four")

packRecord :: SomeRecord -> BS.ByteString
packRecord SomeRecord {..} =
    encodeWithSize recordNumber
        <> encodeWithSize recordString
        <> encodeWithSize (fst recordTuple)
        <> encodeWithSize (snd recordTuple)

someRecordParser :: BS.ByteString -> Either String SomeRecord
someRecordParser =
    execParser
        $   SomeRecord
        <$> extractValue
        <*> extractValue
        <*> ((,) <$> extractValue <*> extractValue)

getSeveralRecords :: [BS.ByteString] -> Either String [SomeRecord]
getSeveralRecords = traverse someRecordParser

instance (Decode a, Decode b) => Decode (a,b) where
    decode = execParser $ (,) <$> extractValue <*> extractValue

instance Decode a => Decode (FileData a) where
    decode =
        execParser
            $   FileData
            <$> extractValue
            <*> extractValue
            <*> extractValue
            <*> extractValue

testRoundTrip :: (Encode a, Decode a, Show a, Eq a) => a -> IO ()
testRoundTrip val = case decode (encode val) of
    Left err -> putStrLn $ "Failed to round-trip value: " <> err
    Right roundTripVal
        | roundTripVal == val -> putStrLn "It works!"
        | otherwise -> do
            putStrLn "Round-trip failed!"
            putStrLn $ "expected: " <> show val
            putStrLn $ "got: " <> show roundTripVal

runRoundTripTest :: IO ()
runRoundTripTest = testRoundTrip $ FileData
    { packedFileName        = "c"
    , packedFileSize        = 8
    , packedFilePermissions = 0644
    , packedFileData        = (0, "zero") :: (Word32, String)
    }

extractValues :: Decode a => FilePackParser [a]
extractValues = FilePackParser $ \input -> if BS.null input
    then Right ([], "")
    else do
        (val , rest ) <- runParser extractValue input
        (tail, rest') <- runParser extractValues rest
        pure (val : tail, rest')

parseEven :: FilePackParser Word32
parseEven = FilePackParser $ \input -> do
    (val, rest) <- runParser extractValue input
    if even val
        then pure (val, rest)
        else Left "Cannot extract a non-even value"

instance Alternative FilePackParser where
    empty = FilePackParser $ const (Left "empty parser")
    parserA <|> parserB = FilePackParser $ \s -> case runParser parserA s of
        Right val  -> Right val
        Left  errA -> runParser parserB s

extractOptional :: FilePackParser a -> FilePackParser (Maybe a)
extractOptional = (<|> pure Nothing) . fmap Just

parseSome :: FilePackParser a -> FilePackParser [a]
parseSome p = (:) <$> p <*> parseMany p

parseMany :: FilePackParser a -> FilePackParser [a]
parseMany p = parseSome p <|> pure []

instance {-# OVERLAPPABLE #-} Decode a => Decode [a] where
    decode = execParser (many extractValue)

testDecodeValue
    :: BS.ByteString
    -> Either
           String
           (FileData String, FileData [Text.Text], FileData (Word32, String))
testDecodeValue =
    execParser $ (,,) <$> extractValue <*> extractValue <*> extractValue

-- From the last chapter
testEncodeValue =
    "\FS\NUL\NUL\NUL\SOH\NUL\NUL\NULa\EOT\NUL\NUL\NUL\ETX\NUL\NUL\NUL\EOT\NUL\NUL\NUL\243\STX\NUL\NUL\ETX\NUL\NUL\NULfoo+\NUL\NUL\NUL\SOH\NUL\NUL\NULb\EOT\NUL\NUL\NUL\n\NUL\NUL\NUL\EOT\NUL\NUL\NUL\132\STX\NUL\NUL\DC2\NUL\NUL\NUL\ENQ\NUL\NUL\NULhello\ENQ\NUL\NUL\NULworld)\NUL\NUL\NUL\SOH\NUL\NUL\NULc\EOT\NUL\NUL\NUL\b\NUL\NUL\NUL\EOT\NUL\NUL\NUL\132\STX\NUL\NUL\DLE\NUL\NUL\NUL\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\NUL\NUL\NULzero"

instance Monad FilePackParser where
    return = pure
    valParser >>= mkParser = FilePackParser $ \input -> do
        (val, rest) <- runParser valParser input
        runParser (mkParser val) rest

data FilePackImage
    = FilePackPBM Word32 Word32 [Word32]
    | FilePackPGM Word32 Word32 Word32 [Word32]
    deriving (Eq, Show)

instance Encode FilePackImage where
    encode (FilePackPBM width height values) =
        encode
            $  encodeWithSize @String "pbm"
            <> encodeWithSize width
            <> encodeWithSize height
        -- The Encode instance for list already includes size info
            <> encode values
    encode (FilePackPGM width height maxValue values) =
        encode
            $  encodeWithSize @String "pgm"
            <> encodeWithSize width
            <> encodeWithSize height
            <> encodeWithSize maxValue
        -- The Encode instance for list already includes size info
            <> encode values

instance Decode FilePackImage where
    decode = execParser $ do
        tag <- extractValue @String
        case tag of
            "pbm" ->
                FilePackPBM
                    <$> extractValue
                    <*> extractValue
                    <*> many extractValue
            "pgm" ->
                FilePackPGM
                    <$> extractValue
                    <*> extractValue
                    <*> extractValue
                    <*> many extractValue
            otherTag -> FilePackParser
                $ \_ -> Left $ "unknown image type tag: " <> otherTag

parsePBM, parsePGM :: FilePackParser FilePackImage
parsePBM = FilePackPBM <$> extractValue <*> extractValue <*> many extractValue
parsePGM =
    FilePackPGM
        <$> extractValue
        <*> extractValue
        <*> extractValue
        <*> many extractValue

getNetpbmParser :: String -> FilePackParser FilePackImage
getNetpbmParser tag = case tag of
    "pbm" -> parsePBM
    "pgm" -> parsePGM
    otherTag ->
        FilePackParser $ \_ -> Left $ "unknown image type tag: " <> otherTag

getNetpbmTag :: FilePackParser String
getNetpbmTag = extractValue

parseImage
    :: FilePackParser String
    -> (String -> FilePackParser FilePackImage)
    -> FilePackParser FilePackImage
parseImage = (>>=)

parseError :: String -> FilePackParser a
parseError errMsg = FilePackParser (const $ Left errMsg)

instance MonadFail FilePackParser where
    fail errMsg = FilePackParser (const $ Left errMsg)

main = do
    print $ testDecodeValue testEncodeValue
