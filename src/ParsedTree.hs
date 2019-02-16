module ParsedTree (
    DT.Tree(..),
    DT.Forest,
    printPretty,
    parser,
    createFromString,
    parserDoc,
    createDoc,
    Psc.ParseError
    ) where

import qualified Data.List as DL
import qualified Data.Tree as DT

import qualified Text.Parsec as Psc
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Char as PscCh
-- import qualified Text.Parsec.Language as PscLang

-- ## Function
isTerminal :: DT.Tree termtype -> Bool
isTerminal DT.Node { DT.subForest = [] } = True
isTerminal _ = False

-- ## Showing
printPrettyInternal :: Show termtype => 
    Int -> DT.Tree termtype -> String
printPrettyInternal indent wholenode@(DT.Node node children)
    | isTerminal wholenode
        = show node
    | otherwise
        = "(" ++ node_str ++ children_str ++ ")"
    where
        node_str :: String
        node_str 
            = (show node) ++ " "
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
            = DL.intercalate 
                children_sep
                (map 
                    (printPrettyInternal children_indent) 
                    children
                )

-- | Provide a string representation of an ABCCategory.
printPretty :: Show termtype => 
    DT.Tree termtype -> String
printPretty tree
    = (printPrettyInternal 0 tree) ++ "\n\n"

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