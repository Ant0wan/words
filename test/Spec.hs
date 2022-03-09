import Test.Hspec
import Lib
import Data

main :: IO ()
main = hspec $ do
  describe "formatGrid" $ do
    it "Should concatenate every line with a newline" $ do
      (formatGrid ["abc","def","ghi"]) `shouldBe` "abc\ndef\nghi\n"

  describe "findWords" $ do
    it "Should find words that exists on the Grid" $ do
      findWord grid "HASKELL" `shouldBe` Just "HASKELL"
      findWord grid "PERL" `shouldBe` Just "PERL"
      findWord grid "RUBY" `shouldBe` Just "RUBY"
    it "Should not find words on the Grid" $ do
      findWord grid "HAMSTER" `shouldBe` Nothing

  describe "findWords" $ do
    it "Should find all the words that exists on the Grid" $ do
      findWords grid languages `shouldBe` languages
    it "Should not find words that do not exists on the Grid" $ do
      findWords grid ["GERMAN", "FRENCH", "TYPESCRIPT"] `shouldBe` []
