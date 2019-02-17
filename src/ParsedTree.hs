module ParsedTree (
    DT.Tree(..),
    DT.Forest,
    getUnary, isUnary,
    justTerminal, isTerminal,
    getNearTerminal, isNearTerminal,
    filterNearTerminal, isFilterNearTerminal,
    parser,
    createFromString,
    parserDoc,
    createDoc,
    Psc.ParseError
    ) where

import qualified Control.Monad as CMon

import qualified Data.List as DL
import qualified Data.Tree as DT
import qualified Data.Maybe as DMay

import qualified Text.Parsec as Psc
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Char as PscCh
-- import qualified Text.Parsec.Language as PscLang

import qualified PTPrintable as PTP

-- ## Function
getUnary :: (DT.Tree term) -> Maybe (DT.Tree term)
getUnary DT.Node { DT.subForest = child:[] } = Just child
getUnary _ = Nothing

isUnary :: (DT.Tree term) -> Bool
isUnary = DMay.isJust . getUnary

justTerminal :: (DT.Tree term) -> Maybe term
justTerminal (DT.Node lex []) = Just lex
justTerminal _ = Nothing 

isTerminal :: (DT.Tree term) -> Bool
isTerminal = DMay.isJust . justTerminal

getNearTerminal :: (DT.Tree term) -> Maybe term
getNearTerminal = getUnary CMon.>=> justTerminal 

isNearTerminal :: (DT.Tree term) -> Bool
isNearTerminal = DMay.isJust . getNearTerminal

filterNearTerminal :: (term -> Bool) -> (DT.Tree term) -> Maybe term
filterNearTerminal cond tree = CMon.mfilter cond $ getNearTerminal tree

isFilterNearTerminal :: (term -> Bool) -> (DT.Tree term) -> Bool
isFilterNearTerminal cond = DMay.isJust . (filterNearTerminal cond)

-- ## Showing
printPrettyInternal ::
    Int 
    -> (term -> String) 
    -> (DT.Tree term) 
    -> String
printPrettyInternal 
    indent
    termPrinter 
    wholenode@(DT.Node node children)
    | isTerminal wholenode
        = termPrinter node
    | otherwise
        = "(" ++ node_str ++ children_str ++ ")"
    where
        node_str :: String
        node_str 
            = (termPrinter node) ++ " "
        node_str_len :: Int
        node_str_len 
            = length node_str
        children_indent :: Int
        children_indent
            = 1 + indent + node_str_len
        children_sep :: String
        children_sep
            = '\n' : concat (replicate children_indent " ")
        children_str :: String
        children_str
            = DL.intercalate children_sep $ map
                (printPrettyInternal children_indent termPrinter) 
                children
                    
                    -- RECURSION

instance (PTP.Printable term) => PTP.Printable (DT.Tree term) where
    psdPrint 
        opt@(PTP.Option PTP.Pretty _)
        tree
        = (printPrettyInternal 0 (PTP.psdPrint opt) tree) ++ "\n\n"

-- ## Parsing
parser :: Parser term -> Parser (DT.Tree term)
parser parserLabel
    = parserTree Psc.<|> parserTerminal
    Psc.<?> "Parsed Tree"
    where
        -- parserTree :: Parser (Tree term)
        parserTree 
            = Psc.between (Psc.char '(') (Psc.char ')') (
                DT.Node 
                <$> (Psc.between Psc.spaces Psc.spaces parserLabel)
                <*> ((parser parserLabel) `Psc.sepBy` Psc.spaces)
                                    -- RECURSION occurring
            ) Psc.<?> "Non-Terminal Node of a Parsed Tree"
        -- parserTerminal :: Parser (Tree term)
        parserTerminal
            = pure <$> parserLabel
            Psc.<?> "Terminal Node of a Parsed Tree"

createFromString :: 
    Parser term 
    -> String 
    -> Either Psc.ParseError (DT.Tree term)
createFromString parserLabel 
    = Psc.parse (parser parserLabel) "Parser of Parsed Trees"

parserDoc :: Parser term -> Parser [DT.Tree term]
parserDoc parserLabel
    = Psc.spaces
        *> (
            ((parser parserLabel) `Psc.sepEndBy` Psc.spaces)
            --(\c -> [c]) <$> (parser parserLabel)
            Psc.<?> "The Content of a Parsed Trees Document"
        ) <* Psc.eof

createDoc :: Parser term -> String -> Either Psc.ParseError [DT.Tree term]
createDoc parserLabel
    = Psc.parse (parserDoc parserLabel) "Parser of Parsed Trees Documents"