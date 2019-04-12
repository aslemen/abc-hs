module ABCCategory.Parser (
    pABCCategory,
    pABCCategoryCommented,
    createABCCategoryCommentedFromString,
    createABCCategoryFromString
    ) where

import qualified Control.Monad.Combinators as CMC
import qualified Control.Monad.Combinators.Expr as CMCE

import Data.Maybe
import Data.Char as DC
import Data.Text as DT

import qualified Text.Megaparsec as TMega

import ABCCategory as ABCC

import LiteralParser as LP
import qualified PTPrintable as PTP

{-
    ======
    Type Definitions
    ======
-}
type Parser = TMega.Parsec () DT.Text

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
            = TMega.takeWhileP (Just "Pure Literal") isCharLiteral
        pBracketedLiteral :: Parser DT.Text
        pBracketedLiteral
            = LP.pBracketedString TMega.<?> "Bracketed Literal"
        isCharLiteral :: Char -> Bool
        isCharLiteral c
            = (not $ DC.isSpace c) && (c `notElem` ".<>()/")

{-
    ------
    ABCCategoryCommented
    ------
-}
pABCCategoryCommented :: Parser ABCC.ABCCategoryCommented
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
pABCCategory :: Parser ABCCategory
pABCCategory
    = CMCE.makeExprParser pTerm opTable TMega.<?> "ABC Category"
    where
        pBaseOrBot :: Parser ABCC.ABCCategory
        pBaseOrBot
            = create <$> pLiteral
            where
                create :: DT.Text -> ABCCategory
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
                    CMCE.InfixL $ TMega.single '\\' *> pure (<\>)
                ]
                ,
                [
                    CMCE.InfixL $ TMega.single '/' *> pure (</>)
                ]
            ]
        pTerm :: Parser ABCCategory
        pTerm 
            = pCatParens pABCCategory 
                TMega.<|> pBaseOrBot 
                TMega.<?> "ABC Category"

{-
    ======
    Parser runner
    ======
-}
createABCCategoryCommentedFromString :: DT.Text -> Either (TMega.ParseErrorBundle DT.Text ()) ABCCategoryCommented
createABCCategoryCommentedFromString 
    = TMega.parse pABCCategoryCommented "<internal>"

createABCCategoryFromString :: DT.Text -> Either (TMega.ParseErrorBundle DT.Text ()) ABCCategory
createABCCategoryFromString 
    = TMega.parse pABCCategory "<internal>"
