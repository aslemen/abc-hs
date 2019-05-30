{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module KaiCat.Parser (
    -- * Parseing Parts
    pKaiCat,
    -- * Constructors Taking Texts
    createKaiCatFromString
    ) where

import qualified Control.Monad.Combinators as CMC
import qualified Control.Monad.Combinators.Expr as CMCE

import qualified Data.Maybe as DMay
import qualified Data.Void as DV
import qualified Data.Char as DC
import qualified Data.Text as DT
import qualified Data.Text.Read as DTR

import qualified Text.Megaparsec as TMega

import qualified ParsedTree.Parser as PTP
import qualified KaiCat as Kai

{-
    ======
    Type Definitions
    ======
-}
type Parser = TMega.Parsec DV.Void DT.Text

{-
    ======
    Parsers
    ======
-}
-- NOTE: NO SPACING

{-
    ------
    Literals
    ------
-}
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
            = TMega.takeWhile1P (Just "Pure Literal") isCharLiteral
        pBracketedLiteral :: Parser DT.Text
        pBracketedLiteral
            = PTP.pBracketedString TMega.<?> "Bracketed Literal"
        isCharLiteral :: Char -> Bool
        isCharLiteral c
            = (not $ DC.isSpace c) 
                && (c `notElem` ("{};" :: [Char]))

{- 
    ------
    KaiCat
    ------
-}

pKaiCat :: Parser Kai.KaiCat
pKaiCat
    = DT.splitOn "-" <$> pLiteral
        >>= \catICH ->
            TMega.option 
                "" 
                (TMega.single ';' *> TMega.takeRest)
            >>= \sort ->
                case DTR.decimal (last catICH) of
                    Right (i, "") -> return Kai.KaiCat {
                        Kai.catlist = init catICH
                        ,
                        Kai.iched = i
                        ,
                        Kai.sortInfo = sort
                    }
                    _ -> return Kai.KaiCat {
                        Kai.catlist = catICH
                        ,
                        Kai.iched = 0
                        ,
                        Kai.sortInfo = sort
                    }

{-
    ======
    Parser runner
    ======
-}
instance PTP.TermParsable Kai.KaiCat where
    getDefaultTermParsers
        = PTP.TermParsers {
            PTP.pTermMany 
                = TMega.option (Kai.createBase [])
                    pKaiCat
            ,
            PTP.pTermSome = pKaiCat
        }

-- | Convert a string into a Kainoki Category.
-- 
-- Examples:
--
-- >>> createKaiCatFromString $ DT.pack "NP-SBJ"
-- Right NP-SBJ
--
-- >>> createKaiCatFromString $ DT.pack "NP-SBJ-2"
-- Right NP-SBJ-2
--
-- >>> createKaiCatFromString $ DT.pack "NP-SBJ;{ABCDD}"
-- Right NP-SBJ;{{ABCDD}}

createKaiCatFromString :: 
    DT.Text 
    -> Either (TMega.ParseErrorBundle DT.Text DV.Void) Kai.KaiCat
createKaiCatFromString 
    = TMega.parse pKaiCat "<internal>"
