module StringWithBracketsSpec where

import qualified Test.Hspec as HS
import qualified Control.Exception as Exc
import qualified Text.Parsec as Psc

import qualified StringWithBrackets as SWB

createSWBFromString = Psc.parse SWB.parserBracketedString ""
createStringORSWB 
    = Psc.parse (SWB.parserStringOrBracketedString "") ""
createManySWB
    = Psc.parse 
        (concat <$> (Psc.many1 $ SWB.parserStringOrBracketedString ""))
        ""

spec :: HS.Spec
spec = do
    HS.describe "parserBracketedString" $ do
        HS.it "parses a bracketed string" $ do
            createSWBFromString "{abc}"
                `HS.shouldBe` Right "{abc}";
            createSWBFromString "{a{bb{{v}"
                `HS.shouldBe` Right "{a{bb{{v}";
            createSWBFromString "{a ads joo3 }"
                `HS.shouldBe` Right "{a ads joo3 }"
        HS.it "parses a bracketed empty string" $ do
            createSWBFromString "{}"
                `HS.shouldBe` Right "{}";

    HS.describe "parserStringOrBracketedString" $ do
        HS.it "parses a bracketed string" $ do
            createStringORSWB "{ABCD}"
                `HS.shouldBe` Right "{ABCD}";
        HS.it "parsers a string" $ do
            createStringORSWB "abbb"
                `HS.shouldBe` Right "abbb";
    HS.describe "many parserStringOrBracketedString" $ do
        HS.it "good concatenation" $ do
            createManySWB "adaf{a ads joo3 }eerere" 
                `HS.shouldBe` Right "adaf{a ads joo3 }eerere";
    
main :: IO ()
main = HS.hspec spec