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
import qualified StringWithBrackets as SWB

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
    = DL.intercalate "-" catList

instance Show KeyakiCategory where
    show all@(KeyakiCategory _ iched sortInfo)
        = (printCatList all)
            ++ "-" ++ iched 
            ++ (";{" ++ sortInfo ++ "}")

instance PTP.Printable KeyakiCategory where
    psdPrint (PTP.Option _ PTP.Minimal) = printCatList
    psdPrint _ = show
    
parser :: Parser KeyakiCategory
parser 
    = do
        catICH <- concat <$> 
            (Psc.many1 
            $ SWB.parserStringOrBracketedString "()|"); -- String
        -- TODO: 普通の;と;{...}との区別。先読みが必要。
        sort <- Psc.option "" (
            Psc.try $
            Psc.between (Psc.string ";{") (Psc.char '}')
                (Psc.many (Psc.noneOf "}"))
            );
        return $ (makeKC $ splitCatICH catICH) { sortInfo = sort }
    where
        splitCatICH :: String -> [String]
        splitCatICH = DLS.splitOn "-" 
        makeKC :: [String] -> KeyakiCategory
        makeKC li
            | length li > 1 && all DC.isDigit (last li) 
                = (createBase (init li)) {iched = last li}
            | otherwise = createBase li

-- | generate an Keyaki-Category from a string or a stream. 
createFromString :: String -> Either Psc.ParseError KeyakiCategory
createFromString 
    = Psc.parse parser "Parser of Keyaki Categories"