module KeyakiCategory (
    KeyakiCategory(..),
    createBase,
    parser,
    createFromString,
    Psc.ParseError,
    ) where

import qualified Data.Char as DC
import qualified Data.List as DL
import qualified Text.Parsec as Psc
import Text.Parsec.String (Parser)

import StringWithBrackets as SWB

data KeyakiCategory =
    KeyakiCategory {
        catlist :: [String],
        iched :: String,
        sort :: String
    }
    deriving (Eq)

createBase :: [String] -> KeyakiCategory
createBase strlist
    = KeyakiCategory {
        catlist = strlist,
        iched = "",
        sort = ""
    }

instance Show KeyakiCategory where
    show KeyakiCategory {catlist = cat}
        = (DL.intercalate "-" cat)

parser :: Parser KeyakiCategory
parser
    = (
        SWB.concatStringWithBrackets 
        <$> Psc.many1 (SWB.parserEitherCharOrBracketedString "()-;")
        ) `Psc.sepBy` (Psc.char '-')
        >>= \catICH ->
            Psc.option "" (
                Psc.between (Psc.string ";{") (Psc.char '}')
                    (Psc.many (Psc.noneOf "}"))
            )
            >>= \sort ->
                return $ (makeKC catICH) { sort = sort }
    where
        makeKC :: [String] -> KeyakiCategory
        makeKC li
            | all DC.isDigit (last li) 
                = (createBase (init li)) {iched = last li}
            | otherwise = createBase li

-- | generate an Keyaki-Category from a string or a stream. 
createFromString :: String -> Either Psc.ParseError KeyakiCategory
createFromString 
    = Psc.parse parser "Parser of Keyaki Categories"