{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ParsedTree.Parser (
    TermParsers(..),
    TermParsable(..),
    parser,
    pBracketedString,
    createFromString,
    parserDoc,
    createDoc,
    TMega.ParseErrorBundle(..)
    ) where

import qualified Data.Char as DCh
import qualified Data.Void as DV
import qualified Data.Set as DS
import qualified Data.Text as DT
import qualified Data.List.NonEmpty as DLNE

import qualified Control.Monad.Combinators as CMC

import qualified Text.Megaparsec as TMega
import qualified Text.Megaparsec.Char as TMegaC
import qualified Text.Megaparsec.Char.Lexer as TMegaCL

import qualified ParsedTree as PT

type Parser = TMega.Parsec DV.Void DT.Text

{-
    ======
    Data Structures
    ======
-}
data TermParsers term
    = TermParsers {
        pTermMany :: Parser term,
        pTermSome :: Parser term
    }

class TermParsable term where
    getDefaultTermParsers :: TermParsers term

instance TermParsable DT.Text where
    getDefaultTermParsers 
        = TermParsers {
            pTermMany 
                = TMega.takeRest
            ,
            pTermSome 
                = TMega.takeWhile1P 
                    (Just "Default Node Label Literal")
                    (const True)
            }
{-
    ======
    Parsers
    ======
-}

{-
    ------
    Lexer components
    ------
-}
pSpaceConsumer :: Parser ()
pSpaceConsumer
    = TMegaCL.space TMegaC.space1 TMega.empty TMega.empty

pLex :: Parser a -> Parser a
pLex = TMegaCL.lexeme pSpaceConsumer

pSymb :: (TMega.Tokens DT.Text) -> Parser (TMega.Tokens DT.Text)
pSymb = TMegaCL.symbol pSpaceConsumer

{-
    ------
    Node Lexers
    ------
-}
pBracketedString :: Parser (TMega.Tokens DT.Text)
pBracketedString
    = TMegaC.char '{'
        >>= \begin ->
            pStrWithBracketsInternal
            >>= \content ->
                TMegaC.char '}'
                >>= \end ->
                    return $ DT.singleton begin <> content <> DT.singleton end
    where
        pStrWithBracketsInternal :: Parser (TMega.Tokens DT.Text)
        pStrWithBracketsInternal
            = mconcat
                <$> CMC.many (
                        pBracketedString  -- RECURSION
                        TMega.<|> 
                        TMega.takeWhile1P
                            (Just "Literal String")
                            (\x -> x /= '{' && x /= '}')
                    )

pStrWithBrackets :: Parser (TMega.Tokens DT.Text)
pStrWithBrackets
    = mconcat 
        <$> CMC.many
        (
            pBracketedString -- RECURSION
            TMega.<|>
            TMega.takeWhile1P
                (Just "Literal String")
                (\x -> x /= '{' && x /= '(' && x /= ')' && not (DCh.isSpace x))
        )

pStrWithBrackets1 :: Parser (TMega.Tokens DT.Text)
pStrWithBrackets1
    = mconcat 
        <$> CMC.some
        (
            pBracketedString -- RECURSION
            TMega.<|>
            TMega.takeWhile1P
                (Just "Literal String")
                (\x -> x /= '{' && x /= '(' && x /= ')' && not (DCh.isSpace x))
        )


{-
    ------
    Tree
    ------
-}
parser :: forall term. TermParsers term -> Parser (PT.Tree term)
parser pTermsGiven
    = pParens pTree 
        TMega.<|> pTermNode
        TMega.<?> "Parsed Tree"
    where
        pParens :: Parser a -> Parser a
        pParens = CMC.between (pSymb "(") (pSymb ")")
        pLabel :: Parser term -> (TMega.State DT.Text) -> Parser term
        pLabel pTerm state -- vertical composition of label parsers
            = case TMega.runParser'
                        (pTerm <* TMega.eof) state of
                (_, Left errors)
                    -> foldr (>>) TMega.empty $
                        map treatError 
                            $ DLNE.toList 
                            $ TMega.bundleErrors errors
                (_, Right res)
                    -> return res
            where
                treatError :: 
                    (TMega.ParseError DT.Text DV.Void)
                        -> Parser term
                treatError (TMega.TrivialError offset unexp exp)
                    = TMega.failure unexp exp
                treatError (TMega.FancyError offset fe)
                    = TMega.fancyFailure fe
        pTree :: Parser (PT.Tree term)
        pTree
            = TMega.getParserState
                >>= \state ->
                    pLex pStrWithBrackets
                        >>= \rootLabel_raw ->
                            pLabel (pTermMany pTermsGiven)
                                state {
                                    TMega.stateInput = rootLabel_raw 
                                    ,
                                    TMega.stateOffset = 0
                                    }
                                >>= \rootLabel ->
                                    CMC.many (pLex $ parser pTermsGiven)
                                                         -- RECURSION
                                    >>= \children ->
                                        return $ PT.Node rootLabel children
        pTermNode :: Parser (PT.Tree term)
        pTermNode
            = TMega.getParserState
                >>= \state ->
                    pLex pStrWithBrackets
                        >>= \label_raw ->
                            pLabel 
                                (pTermSome pTermsGiven) 
                                state {
                                    TMega.stateInput = label_raw
                                    ,
                                    TMega.stateOffset = 0
                                }
                                >>= \label -> return $ PT.Node label []
createFromString ::
    TermParsers term
        -> String
        -> DT.Text
        -> Either (TMega.ParseErrorBundle DT.Text DV.Void) (PT.Tree term)
createFromString pTermsGiven
    = TMega.parse (parser pTermsGiven)


{-
    ------
    Documents
    ------
-}
parserDoc :: TermParsers term -> Parser [PT.Tree term]
parserDoc pTermsGiven
    = pLex TMegaC.space
        >> (TMega.many $ pLex $ parser pTermsGiven)
        >>= \doc ->
            TMega.eof
            >> return doc

createDoc ::
    TermParsers term
        -> String
        -> DT.Text
        -> Either (TMega.ParseErrorBundle DT.Text DV.Void) [PT.Tree term]
createDoc pTermsGiven
    = TMega.parse (parserDoc pTermsGiven)