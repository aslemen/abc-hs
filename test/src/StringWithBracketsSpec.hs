import qualified Test.Hspec as HS
import qualified Control.Exception as Exc
import qualified Text.Parsec as Psc

import qualified StringWithBrackets as SWB

createSWBFromString = Psc.parse SWB.parserBracketedString ""
createSWBOrChar 
    = Psc.parse (SWB.parserEitherCharOrBracketedString "") ""

spec :: HS.Spec
spec = do
    HS.describe "parserBracketedString" $ do
        HS.it "parses a bracketed string" $ do
            createSWBFromString "{abc}"
                `HS.shouldBe` (Right $ SWB.BracketedString "abc");
            createSWBFromString "{a{bb{{v}"
                `HS.shouldBe` (Right $ SWB.BracketedString "a{bb{{v")
        HS.it "parses a bracketed empty string" $ do
            createSWBFromString "{}"
            `HS.shouldBe` (Right $ SWB.BracketedString "")

    HS.describe "parserEitherCharOrBracketedString" $ do
        HS.it "parses a bracketed string" $ do
            createSWBOrChar "{ABCD}"
            `HS.shouldBe` (Right $ Right $ SWB.BracketedString "ABCD")
        HS.it "parsers a letter" $ do
            createSWBOrChar "a"
            `HS.shouldBe` (Right $ Left 'a')
main :: IO ()
main = HS.hspec spec