import           HCat
import qualified System.IO.Error               as IOError
import           Test.Hspec
import           Test.QuickCheck

test_eitherToErr :: Spec
test_eitherToErr = do
    context "When called with a Right value"
        $ it "Doesn't raise an exception"
        $ property
        $ \val ->
              let val' = Right val :: Either String Int
              in  HCat.eitherToErr val' `shouldReturn` val
    context "When called with a Left value"
        $ it "Throws an IO exception"
        $ property
        $ \err ->
              let eitherErrVal = Left err :: Either String ()
                  expected     = IOError.userError (show err)
              in  HCat.eitherToErr eitherErrVal `shouldThrow` (== expected)

main :: IO ()
main = hspec test_eitherToErr
