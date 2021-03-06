import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import FileParse

-- fakeFile :: String -> IO String
-- fakeFile fileName = return "Fake content"

main :: IO ()
main = hspec $ do
  describe "FileParse" $ do
      describe "checkEndOfStatement" $ do
        it "should return true when passed a ';' character" $ do
          checkEndOfStatement ';' `shouldBe` True

        it "should return false when passed a character other than ';'" $ do
          checkEndOfStatement 'f' `shouldBe` False

      describe "getNextStatememt" $ do
        it "should return the ';' character when passed a ';' character" $ do
          (getNextStatement "" "" ";") `shouldBe` (FileParsingInformation "" ";" "")

        it "should append the ';' character when the next character is ';'" $ do
          (getNextStatement "" "HELLO WORLD" ";") `shouldBe` (FileParsingInformation "" "HELLO WORLD;" "")

        it "should pass all values inclusively when it contains the ';' character" $ do
          (getNextStatement "" "" "HELLO WORLD;") `shouldBe` (FileParsingInformation "" "HELLO WORLD;" "")

        it "should pass the statement back if there are no statements left to parse" $ do
          (getNextStatement "" "HELLO WORLD" "") `shouldBe` (FileParsingInformation "" "HELLO WORLD" "")

        it "should pass all values inclusively when there are no other splitting characters in the string" $ do
          (getNextStatement "" "" "HELLO WORLD;") `shouldBe` (FileParsingInformation "" "HELLO WORLD;" "")

        it "should pass all values inclusively when it contains the ';' character" $ do
          (getNextStatement "" "" "HELLO WORLD; WORLD, HELLO") `shouldBe` (FileParsingInformation "" "HELLO WORLD;" " WORLD, HELLO")