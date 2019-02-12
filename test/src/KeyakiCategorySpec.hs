module KeyakiCategorySpec where

import qualified Test.Hspec as HS
import qualified Control.Exception as Exc
import qualified Text.Parsec as Psc

import qualified KeyakiCategory as K
spec :: HS.Spec
spec = do
    HS.describe "createFromString" $ do
        HS.it "parse a Keyaki Category (Cat, ICHed, Sort)" $ do
            K.createFromString "AB-B-CCJI-3;{{}"
                `HS.shouldBe` Right K.KeyakiCategory {
                    K.catlist = ["AB", "B", "CCJI"],
                    K.iched = "3",
                    K.sortInfo = "{"
                };
        HS.it "parse a Keyaki Category (Cat, ICHed)" $ do
            K.createFromString "AB-B-CCJI-3"
                `HS.shouldBe` Right K.KeyakiCategory {
                    K.catlist = ["AB", "B", "CCJI"],
                    K.iched = "3",
                    K.sortInfo = ""
                };
        HS.it "parse a Keyaki Category (Cat)" $ do
            K.createFromString "AB-B-CCJI"
                `HS.shouldBe` Right K.KeyakiCategory {
                    K.catlist = ["AB", "B", "CCJI"],
                    K.iched = "",
                    K.sortInfo = ""
                };
        HS.it "parse a Keyaki Category (Cat, Sort)" $ do
            K.createFromString "AB-B-CCJI;{A   BC}"
                `HS.shouldBe` Right K.KeyakiCategory {
                    K.catlist = ["AB", "B", "CCJI"],
                    K.iched = "",
                    K.sortInfo = "A   BC"
                };
main :: IO ()
main = HS.hspec spec