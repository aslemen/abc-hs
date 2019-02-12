module StringWithBrackets (
    parserBracketedString,
    parserStringOrBracketedString,
    ) where

import Control.Applicative
import Data.Maybe
import Data.Char as DCh

import qualified Text.Parsec as Psc
import Text.Parsec.String (Parser)
-- import qualified Text.Parsec.Language as PscLang
import qualified Text.Parsec.Expr as PscExpr

parserBracketedString :: Parser String
parserBracketedString
    = Psc.char '{'
        >>= \open ->
            Psc.many (Psc.noneOf "}")
            >>= \content ->
                Psc.char '}'
                >>= \close ->
                    return (open : (content ++ [close]))
        Psc.<?> "Bracketed String"

parserStringOrBracketedString :: 
    [Char] -> Parser String
parserStringOrBracketedString constraints
    = parserBracketedString
        Psc.<|>
        (Psc.many1 $ Psc.satisfy charCondition)
    where
        charCondition :: Char -> Bool
        charCondition c
            = not (
                DCh.isSpace (c :: Char)
                || (c `elem` "{}" ++ constraints)
            )