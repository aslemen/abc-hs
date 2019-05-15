{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module KaiCat.Parser (

) where

import qualified Control.Monad.Combinators as CMC
import qualified Control.Monad.Combinators.Expr as CMCE

import qualified Data.Maybe as DMay
import qualified Data.Void as DV
import qualified Data.Char as DC
import qualified Data.Text as DT

import qualified Text.Megaparsec as TMega

import qualified ParsedTree.Parser as PTP
import qualified ABCCategory as ABCC

parserBracketedString :: Parser String
parserBracketedString
    = (
        Psc.between (Psc.char '{') (Psc.char '}') 
        $ Psc.many (Psc.noneOf "}")
    ) Psc.<?> "Bracketed String"


parserString :: Parser String
parserString
    = (Psc.many1 parserNonEscapeChars)
        Psc.<?> "Non-Bracketed String"
    where 
        condition :: Char -> Bool
        condition c
            = not $ DC.isSpace c || c `elem` ";{}()-'"
        parserNonEscapeChars :: Parser Char
        parserNonEscapeChars
            = Psc.satisfy condition

parserSemicolon :: Parser String
parserSemicolon
    = (Psc.notFollowedBy $ Psc.string ";{")
        *> Psc.string ";" Psc.<?> "Semicolon"

parser :: Parser KaiCat
parser 
    =  parserCatICH
        >>= \catICH ->
            Psc.option "" (
                Psc.between (Psc.string ";{") (Psc.char '}')
                    $ Psc.many (Psc.noneOf "}")
            )
            >>= \sort ->
                return $ catICH { sortInfo = sort }
    where
        parserStringOrBracketedString :: Parser String
        parserStringOrBracketedString
            = Psc.many1 (
                parserSemicolon 
                Psc.<|> parserBracketedString 
                Psc.<|> parserString
                )
                >>= (return . concat)
        makeKC :: [String] -> KaiCat
        makeKC li
            | length li > 1 && all DC.isDigit (last li) 
                = (createBase (init li)) { iched = last li }
            | otherwise 
                = createBase li
        parserCatICH :: Parser KaiCat
        parserCatICH
            = makeKC 
                <$> (
                    parserStringOrBracketedString `Psc.sepBy1` (Psc.char '-')
                )

-- | generate an Keyaki-Category from a string or a stream. 
createFromString :: String -> Either Psc.ParseError KaiCat
createFromString 
    = Psc.parse parser "Parser of Keyaki Categories"