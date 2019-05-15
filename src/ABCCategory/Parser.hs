{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module ABCCategory.Parser (
    pABCCategory,
    pABCCategoryCommented,
    createABCCategoryCommentedFromString,
    createABCCategoryFromString
    ) where

import qualified Control.Monad.Combinators as CMC
import qualified Control.Monad.Combinators.Expr as CMCE

import Data.Maybe
import Data.Void as DV
import Data.Char as DC
import Data.Text as DT

import qualified Text.Megaparsec as TMega

import qualified ParsedTree.Parser as PTP
import qualified ABCCategory as ABCC

import qualified ABCComment as Com
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
            = (not $ DC.isSpace c) && (c `notElem` (".<>/\\" :: [Char]))

{-
    ------
    ABCCategoryCommented
    ------
-}
pABCCategoryCommented :: Parser (Com.ABCComment ABCC.ABCCategory)
pABCCategoryCommented
    = ABCC.ABCComment <$> pABCCategory <*> pCommentOrEmpty
    where 
        pCommentOrEmpty :: Parser DT.Text
        pCommentOrEmpty
            = CMC.option DT.empty pABCComment

pABCComment :: Parser DT.Text
pABCComment
    = TMega.single '.'
        *> TMega.takeWhileP (Just "Comment") isCommentChar
    where
        isCommentChar :: Char -> Bool
        isCommentChar c = (DC.isLetter c) || (c == '\"')

{-
    ------
    ABCCategory
    ------
-}
pABCCategory :: Parser ABCC.ABCCategory
pABCCategory
    = CMCE.makeExprParser pTerm opTable TMega.<?> "ABC Category"
    where
        pBaseOrBot :: Parser ABCC.ABCCategory
        pBaseOrBot
            = create <$> pLiteral
            where
                create :: DT.Text -> ABCC.ABCCategory
                create text
                    | text == ABCC.strBot
                        = ABCC.Bottom
                    | otherwise
                        = ABCC.BaseCategory text
        pCatParens :: Parser a -> Parser a
        pCatParens = CMC.between (TMega.single '<') (TMega.single '>')
        opTable :: [[CMCE.Operator Parser ABCC.ABCCategory]]
        opTable 
            = [
                [
                    CMCE.InfixL $ TMega.single '\\' *> pure (ABCC.<\>)
                ]
                ,
                [
                    CMCE.InfixL $ TMega.single '/' *> pure (ABCC.</>)
                ]
            ]
        pTerm :: Parser ABCC.ABCCategory
        pTerm 
            = pCatParens pABCCategory 
                TMega.<|> pBaseOrBot 
                TMega.<?> "ABC Category"

{-
    ======
    Parser runner
    ======
-}
instance PTP.TermParsable ABCC.ABCCategory where
    getDefaultTermParsers
        = PTP.TermParsers {
            PTP.pTermMany 
                = TMega.option (ABCC.BaseCategory "")
                    pABCCategory
            ,
            PTP.pTermSome = pABCCategory
        }

instance PTP.TermParsable (Com.ABCComment ABCC.ABCCategory) where
    getDefaultTermParsers 
        = PTP.TermParsers {
            PTP.pTermMany 
                = TMega.option
                    (
                        Com.ABCComment {
                            Com.content = ABCC.BaseCategory ""
                            ,
                            Com.comment = ""
                        }
                    )
                    pABCCategoryCommented
            ,
            PTP.pTermSome = pABCCategoryCommented
        }

createABCCategoryCommentedFromString :: 
    DT.Text 
    -> Either 
        (TMega.ParseErrorBundle DT.Text DV.Void)
        (Com.ABCComment ABCC.ABCCategory)
createABCCategoryCommentedFromString 
    = TMega.parse pABCCategoryCommented "<internal>"

createABCCategoryFromString :: DT.Text -> Either (TMega.ParseErrorBundle DT.Text DV.Void) ABCC.ABCCategory
createABCCategoryFromString 
    = TMega.parse pABCCategory "<internal>"
