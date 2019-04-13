{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module DepMarked.Parser (
    pPlainMarked
    ) where

import qualified Control.Monad.Combinators as CMC

import qualified Data.Char as DC
import qualified Data.Text as DT
import qualified Data.Void as DV

import qualified Text.Megaparsec as TMega

import qualified DepMarking as DMing
import qualified DepMarked as DMed

import qualified ParsedTree.Parser as PTP

type Parser = TMega.Parsec DV.Void DT.Text

pLiteral :: Parser DT.Text
pLiteral
    = DT.concat
        <$> CMC.some
            (
                pPureLiteral TMega.<|> pBracketedLiteral
            )
        TMega.<?> "Literal"
    where
        pPureLiteral :: Parser DT.Text
        pPureLiteral
            = TMega.takeWhile1P
                (Just "Pure Literal")
                (\c -> (not $ DC.isSpace c) && c /= '\'')
        pBracketedLiteral :: Parser DT.Text
        pBracketedLiteral
            = PTP.pBracketedString

pPlainMarked :: Parser (DMed.DepMarked DT.Text)
pPlainMarked
    = pLiteral
        >>= \main ->
            CMC.option (DMed.:| DMing.None) pMarked
            >>= \markfunc ->
                return $ markfunc main
    where
        pMarked :: Parser (DT.Text -> DMed.DepMarked DT.Text)
        pMarked
            = DMed.markCat 
                <$> (
                    TMega.chunk DMed.marker
                        >> TMega.takeWhile1P 
                            (Just "Dependency Marking")
                            (const True)
                    )

instance PTP.TermParsable (DMed.DepMarked DT.Text) where
    getDefaultTermParsers
        = PTP.TermParsers {
            PTP.pTermMany
                = CMC.option
                    ("" DMed.:| DMing.None)
                    pPlainMarked
            ,
            PTP.pTermSome
                = pPlainMarked
        }
