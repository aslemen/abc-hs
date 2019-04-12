{-# LANGUAGE OverloadedStrings #-}

module LiteralParser (
    pBracketedString
    ) where

import qualified Data.Char as DCh
import qualified Data.Text as DT

import qualified Text.Megaparsec as TMega
import qualified Text.Megaparsec.Char as TMegaC

type Parser = TMega.Parsec () DT.Text

pBracketedString :: Parser (TMega.Tokens DT.Text)
pBracketedString
    = (DT.singleton <$> TMegaC.char '{')
        <++> (TMega.takeWhileP (Just "Bracketed String") (/= '}'))
        <++> (DT.singleton <$> TMegaC.char '}')
    where
        (<++>) :: Parser (TMega.Tokens DT.Text) -> Parser (TMega.Tokens DT.Text) ->  Parser (TMega.Tokens DT.Text)
        (<++>) = (<*>) . fmap (<>)