module ABCCategory (
    ABCCategory(antecedent, consequence, name, comment),
    strBot,
    createBot,
    createBase,
    (<<-),
    (->>),
    reduce,
    reduceWithComment,
    parser,
    createFromString,
    Psc.ParseError
    ) where

import qualified Text.Parsec as Psc
import Text.Parsec.String (Parser)
-- import qualified Text.Parsec.Language as PscLang
import qualified Text.Parsec.Expr as PscExpr
import Control.Applicative ((<$>), (<*>))
import Data.Maybe

-- # The Data Type
data ABCCategory = 
      Bottom {
        comment :: String
        }
    | BaseCategory {
        name :: String,
        comment :: String
        }
    | LeftFunctor { 
        antecedent :: ABCCategory, 
        consequence :: ABCCategory,
        comment :: String
        }
    | RightFunctor { 
        antecedent :: ABCCategory, 
        consequence :: ABCCategory,
        comment :: String
        }

strBot :: String
strBot = "⊥"

-- ## Aliases of the constructors
createBot :: ABCCategory
createBot = Bottom {comment = ""}

createBase :: String -> ABCCategory
createBase x = BaseCategory {name = x, comment = ""}

(->>) :: ABCCategory -> ABCCategory -> ABCCategory
ant ->> conseq 
    = LeftFunctor {
        antecedent = ant,
        consequence = conseq,
        comment = ""
        }

(<<-) :: ABCCategory -> ABCCategory -> ABCCategory
conseq <<- ant 
    = RightFunctor {
        antecedent = ant, 
        consequence = conseq,
        comment = ""
        }

-- ## Equation
instance Eq ABCCategory where
    -- | The equation of ABCCategories ignores comments.
    (==) (Bottom _) (Bottom _) 
        = True
    (==) (BaseCategory x _) (BaseCategory y _) 
        = (x == y)
    (==) (LeftFunctor ant1 conseq1 _) (LeftFunctor ant2 conseq2 _)
        = (ant1 == ant2) && (conseq1 == conseq2)
    (==) (RightFunctor ant1 conseq1 _) (RightFunctor ant2 conseq2 _)
        = (ant1 == ant2) && (conseq1 == conseq2)
    (==) _ _ 
        = False

-- ## Showing

-- | Provide a string representation of an ABCCategory.
printCategory :: Bool -> ABCCategory -> String
printCategory isComment cat
    = case cat of
        Bottom x
            -> strBot ++ (showComment isComment x)
        BaseCategory name x
            -> name ++ (showComment isComment x)
        LeftFunctor ant conseq x
            -> "<" ++ (show ant) ++ "/" ++ (show conseq) ++ ">"
                     ++ (showComment isComment x)
        RightFunctor ant conseq x
            -> "<" ++ (show conseq) ++ "\\" ++ (show ant) ++ ">" 
                    ++ (showComment isComment x)
    where
        makeComment :: String -> String
        makeComment str
            | str == "" = ""
            | otherwise = ".\"" ++ str ++ "\""
        showComment :: Bool -> String -> String
        showComment isComment
            | True = makeComment
            | False = \_ -> ""

instance Show ABCCategory where
    show = printCategory True

-- ## Reduction
data ABCStatusFC = FCLeft Int | FCRight Int | None

reduceInner :: ABCCategory -> ABCCategory -> (ABCCategory, ABCStatusFC)
reduceInner (BaseCategory name _) (LeftFunctor (BaseCategory ant _) conseq _) -- X A/B
    = if name == ant
        then (conseq, FCLeft 0) 
        else (createBot, None)
reduceInner left@(LeftFunctor ant1 conseq1 _) (LeftFunctor ant2 conseq2 _)
    -- A/B C/D
    | conseq1 == ant2 
        = (ant1 ->> conseq2, FCLeft 1) -- A/B B/D => A/D
    | otherwise 
        = case deeper_num of
            FCLeft n
                -> (deeper_cat ->> conseq2, FCLeft (n + 1))
            _ -> (createBot, None)
        where 
            deeper_res :: (ABCCategory, ABCStatusFC)
            deeper_res = reduceInner left ant2 
                -- A/B C where C might be X/Y
            deeper_cat :: ABCCategory
            deeper_cat = fst deeper_res
            deeper_num :: ABCStatusFC
            deeper_num = snd deeper_res
reduceInner (RightFunctor (BaseCategory ant _) conseq _) (BaseCategory name _) -- A\B X
    = if name == ant
        then (conseq, FCRight 0) 
        else (createBot, None)
reduceInner (RightFunctor ant1 conseq1 _) right@(RightFunctor ant2 conseq2 _)
    | conseq2 == ant1 
        = (conseq1 <<- ant2, FCRight 1) -- A\B C\D, B=C => A\D
    | otherwise 
        = case deeper_num of
            FCRight n
                -> (conseq1 <<- deeper_cat, FCRight (n + 1))
            _ -> (createBot, None)
        where 
            deeper_res :: (ABCCategory, ABCStatusFC)
            deeper_res = reduceInner ant1 right
                -- B C\D where B might be X\Y
            deeper_cat :: ABCCategory
            deeper_cat = fst deeper_res
            deeper_num :: ABCStatusFC
            deeper_num = snd deeper_res
reduceInner _ _
    = (createBot, None)

reduce :: ABCCategory -> ABCCategory -> ABCCategory
reduce cat1 cat2 = fst (reduceInner cat1 cat2)

reduceWithComment :: ABCCategory -> ABCCategory -> ABCCategory
reduceWithComment cat1 cat2
    = cat {comment = makeComment num}
    where
        res :: (ABCCategory, ABCStatusFC)
        res = reduceInner cat1 cat2
        cat :: ABCCategory
        cat = fst res
        num :: ABCStatusFC
        num = snd res
        makeComment :: ABCStatusFC -> String
        makeComment (FCLeft n)
            | n > 0     = "FCLeft" ++ show n
            | otherwise = "L"
        makeComment (FCRight n)
            | n > 0     = "FCRight" ++ show n
            | otherwise = "R"
        makeComment None
            = ""

-- ## Parsing
parserBaseOrBot :: Parser ABCCategory
parserBaseOrBot = 
    (Psc.many1 Psc.letter)
    >>= \str ->
        if str == strBot
            then return createBot 
            else return (createBase str)

opTableComplex = [
    [PscExpr.Infix (Psc.char '/' >> return (->>)) PscExpr.AssocRight],
    [PscExpr.Infix (Psc.char '\\' >> return (<<-)) PscExpr.AssocLeft]
    ]

parserCommentMaybe :: Parser String
parserCommentMaybe =
    Psc.optionMaybe parserComment
    >>= \res -> 
        return (printComment (res :: Maybe String))
    where
        parserComment :: Parser String
        parserComment = 
            Psc.string ".\""
            >> Psc.many (Psc.noneOf "\"")
            >>= \res ->
                Psc.char '\"'
                >> return (res :: String)
        printComment :: Maybe String -> String
        printComment (Just x) = x
        printComment Nothing = ""

parser :: Parser ABCCategory
parser
    = PscExpr.buildExpressionParser opTableComplex parserComplex
    where
        parserFunc :: Parser ABCCategory
        parserFunc
            = Psc.char '<'
                >> parser
                >>= \cat ->
                    Psc.char '>'
                    >> return cat
        parserComplex :: Parser ABCCategory
        parserComplex
            = (parserFunc Psc.<|> parserBaseOrBot)
                >>= \cat -> 
                    parserCommentMaybe
                    >>= \com ->
                        return cat {comment = com}
                Psc.<?> "ABC Cagetory"

-- | generate an ABC-Category from a string or a stream. 
createFromString :: String -> Either Psc.ParseError ABCCategory
createFromString 
    = Psc.parse parser "ABC Cagetory"