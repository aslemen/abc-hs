module KeyakiCategory (
    KeyakiCategory(..),
    createBase,
    parser,
    createFromString,
    Psc.ParseError,
    ) where

import qualified Data.Char as DC
import qualified Data.List as DL
import qualified Data.List.Split as DLS
import qualified Text.Parsec as Psc
import Text.Parsec.String (Parser)

import qualified PTPrintable as PTP

data KeyakiCategory =
    KeyakiCategory {
        catlist :: [String],
        iched :: String,
        sortInfo :: String
    }
    deriving (Eq)

createBase :: [String] -> KeyakiCategory
createBase strlist
    = KeyakiCategory {
        catlist = strlist,
        iched = "",
        sortInfo = ""
    }

printCatList :: KeyakiCategory -> String
printCatList (KeyakiCategory catList _ _)
    = DL.intercalate "-" $ escapedCat <$> catList
    where 
        isToBeEscaped :: String -> Bool
        isToBeEscaped
            = any (\c -> DC.isSpace c || c `elem` "()-|")
        escapedCat :: String -> String
        escapedCat str
            = if isToBeEscaped str
                then '{' : (str ++ "}")
                else str -- '\"' : (str ++ "\"")

instance Show KeyakiCategory where
    show all@(KeyakiCategory _ iched sortInfo)
        = (printCatList all)
            ++ "-" ++ iched 
            ++ (";{" ++ sortInfo ++ "}")

instance PTP.Printable KeyakiCategory where
    psdPrint (PTP.Option _ PTP.Minimal) 
        = printCatList
    psdPrint _ 
        = show

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
            = not $ DC.isSpace c || c `elem` ";{}()-|"
        parserNonEscapeChars :: Parser Char
        parserNonEscapeChars
            = Psc.satisfy condition

parserSemicolon :: Parser String
parserSemicolon
    = (Psc.notFollowedBy $ Psc.string ";{")
        *> Psc.string ";" Psc.<?> "Semicolon"

parser :: Parser KeyakiCategory
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
        makeKC :: [String] -> KeyakiCategory
        makeKC li
            | length li > 1 && all DC.isDigit (last li) 
                = (createBase (init li)) { iched = last li }
            | otherwise 
                = createBase li
        parserCatICH :: Parser KeyakiCategory
        parserCatICH
            = makeKC 
                <$> (
                    parserStringOrBracketedString `Psc.sepBy1` (Psc.char '-')
                )

-- | generate an Keyaki-Category from a string or a stream. 
createFromString :: String -> Either Psc.ParseError KeyakiCategory
createFromString 
    = Psc.parse parser "Parser of Keyaki Categories"