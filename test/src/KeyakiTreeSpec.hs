module KeyakiTreeSpec where

import qualified Test.Hspec as HS
import qualified Control.Exception as Exc
import qualified Text.Parsec as Psc

import qualified DepMarking as DMing
import qualified DepMarked as DMed
import qualified ParsedTree as PT
import qualified KeyakiCategory as KC

parserKTMarked = DMed.parser KC.parser
parseKTM = Psc.parse parserKTMarked ""

-- runParserKTMarked :: String -> Either PT.ParseError KTMarked
runParserKTMarked
    = PT.createFromString parserKTMarked

spec :: HS.Spec
spec = do
    HS.describe "parse a Keyaki Cat With Dep Marking" $ do
        HS.it "parse a cat" $ do
            parseKTM "A-BB-C|h"
                `HS.shouldBe` (
                    Right $ 
                    KC.createBase ["A", "BB", "C"] DMed.:| DMing.Head);
    HS.describe "parse a Keyaki Tree" $ do
        HS.it "parser a tree" $ do
            runParserKTMarked "(A|a B|c C|h)"
                `HS.shouldBe` (Right $
                    PT.Node 
                        (KC.createBase ["A"] DMed.:| DMing.Adjunct)
                        [
                            PT.Node 
                                (KC.createBase ["B"] DMed.:| DMing.Complement)
                             [],
                            PT.Node (KC.createBase ["C"] DMed.:| DMing.Head)
                            []
                        ]
                );
main :: IO ()
main = HS.hspec spec