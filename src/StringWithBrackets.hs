module StringWithBrackets (
    BracketedString,
    StringWithBrackets,
    concatStringWithBrackets,
    parserBracketedString,
    parserEitherCharOrBracketedString
    ) where

import Control.Applicative
import Data.Maybe
import Data.Char as DCh

import qualified Text.Parsec as Psc
import Text.Parsec.String (Parser)
-- import qualified Text.Parsec.Language as PscLang
import qualified Text.Parsec.Expr as PscExpr

newtype BracketedString = BracketedString String

instance Show BracketedString where
    show (BracketedString str) = "{" ++ str ++ "}"

type EitherCharOrBracketedString = Either Char BracketedString

type StringWithBrackets = [EitherCharOrBracketedString]

concatStringWithBrackets :: StringWithBrackets -> String
concatStringWithBrackets ((Left char):swbs)
    = char : (concatStringWithBrackets swbs)
concatStringWithBrackets ((Right swb):swbs)
    = (show swb) ++ (concatStringWithBrackets swbs)
concatStringWithBrackets [] 
    = ""

parserBracketedString :: Parser BracketedString
parserBracketedString
    = BracketedString 
        <$> Psc.between (Psc.char '{') (Psc.char '}')
                (Psc.many (Psc.satisfy (/= '}')))
        Psc.<?> "Bracketed String"

parserEitherCharOrBracketedString :: 
    [Char] -> Parser EitherCharOrBracketedString
parserEitherCharOrBracketedString constraints
    = (Right <$> parserBracketedString)
        Psc.<|> 
        (Left <$> Psc.satisfy ( \c -> 
            not (
                DCh.isSpace (c :: Char)
                || (c `elem` "{}" ++ constraints)
            )
            ) Psc.<?> "Letter"
        )