module ABCCategory (
    ABCCategory(antecedent, consequence, name, comment),
    strBot,
    createBot,
    createBase,
    (</>),
    (<\>),
    (<^>),
    addComment,

    ABCStatusFC(..),
    reduceWithResult,

    parser,
    createFromString,
    Psc.ParseError
    ) where

import Control.Applicative
import Data.Maybe
import Data.Char as DCh

import qualified Text.Parsec as Psc
import Text.Parsec.String (Parser)
-- import qualified Text.Parsec.Language as PscLang
import qualified Text.Parsec.Expr as PscExpr

import StringWithBrackets as SWB
import qualified PTPrintable as PTP

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

(<\>) :: ABCCategory -> ABCCategory -> ABCCategory
ant <\> conseq 
    = LeftFunctor {
        antecedent = ant,
        consequence = conseq,
        comment = ""
        }

(</>) :: ABCCategory -> ABCCategory -> ABCCategory
conseq </> ant 
    = RightFunctor {
        antecedent = ant, 
        consequence = conseq,
        comment = ""
        }

addComment :: ABCCategory -> String -> ABCCategory
cat `addComment` str = cat { comment = str }

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
            -> "<" ++ (show ant) ++ "\\" ++ (show conseq) ++ ">"
                     ++ (showComment isComment x)
        RightFunctor ant conseq x
            -> "<" ++ (show conseq) ++ "/" ++ (show ant) ++ ">" 
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

instance PTP.Printable ABCCategory where
    psdPrint (PTP.Option _ PTP.Minimal) = printCategory False
    psdPrint _ = show

    -- ## Reduction
data ABCStatusFC = FCLeft Int | FCRight Int | Failed
instance Eq ABCStatusFC where
    (==) (FCLeft n) (FCLeft m) 
        = n == m
    (==)
        (FCRight n) (FCRight m)
        = n == m
    (==) Failed Failed
        = True
    (==) _ _
        = False
instance Show ABCStatusFC where
    show (FCLeft n)
        | n > 0     = "FCLeft" ++ show n
        | otherwise = "L"
    show (FCRight n)
        | n > 0     = "FCRight" ++ show n
        | otherwise = "R"
    show Failed
        = "FAIL"

catStatFailed :: (ABCCategory, ABCStatusFC)
catStatFailed = (createBot, Failed)

reduceWithResult :: ABCCategory -> ABCCategory -> (ABCCategory, ABCStatusFC)
reduceWithResult base@(BaseCategory _ _) (LeftFunctor ant2 conseq2 _)
    | base == ant2
        = (conseq2, FCLeft 0)
    | otherwise
        = catStatFailed
reduceWithResult (RightFunctor ant1 conseq1 _) base@(BaseCategory _ _)
    | ant1 == base
        = (conseq1, FCRight 0)
    |otherwise
        = catStatFailed
reduceWithResult 
    left@(LeftFunctor ant1 conseq1 _)
    right@(LeftFunctor ant2 conseq2 _)
    | left == ant2
        = (conseq2, FCLeft 0)
    | otherwise
        = case dres of
            FCLeft n
                -> (ant1 <\> dcat, FCLeft (n + 1))
            _
                -> catStatFailed
            where
                dcat :: ABCCategory
                dres :: ABCStatusFC
                (dcat, dres) = reduceWithResult conseq1 right
reduceWithResult 
    left@(RightFunctor ant1 conseq1 _)
    right@(RightFunctor ant2 conseq2 _)
    | ant1 == right
        = (conseq1, FCRight 0)
    | otherwise
        = case dres of
            FCRight n
                -> (dcat </> ant2, FCRight (n + 1))
            _
                -> catStatFailed
            where
                dcat :: ABCCategory
                dres :: ABCStatusFC
                (dcat, dres) = reduceWithResult left conseq2
reduceWithResult 
    left@(RightFunctor ant1 conseq1 _)
    right@(LeftFunctor ant2 conseq2 _) -- conseq1/ant1 ant2\conseq2
    | left == ant2
        = (conseq2, FCLeft 0)
    | ant1 == right
        = (conseq1, FCRight 0)
    | otherwise
        = catStatFailed
reduceWithResult _ _ 
    = catStatFailed

(<^>) :: ABCCategory -> ABCCategory -> ABCCategory
cat1 <^> cat2 
    = fst (reduceWithResult cat1 cat2)

-- ## Parsing
parser :: Parser ABCCategory
parser
    = PscExpr.buildExpressionParser opTableComplex parserComplex
    where
        parserBaseOrBot :: Parser ABCCategory
        parserBaseOrBot = 
            create . concat 
            <$> Psc.many1 
                (SWB.parserStringOrBracketedString ".<>()/\\")
                Psc.<?> "Base ABC Category"
            where
                create :: String -> ABCCategory
                create str
                    | str == strBot 
                        = createBot
                    | otherwise
                        = createBase str
        opTableComplex = [
            [
                PscExpr.Infix
                    (
                        (Psc.char '\\' 
                            Psc.<?> "Left Functor in an ABC Category")
                        *> pure (<\>)
                    )
                    PscExpr.AssocRight
            ],
            [
                PscExpr.Infix
                    (
                        (Psc.char '/'
                            Psc.<?> "Right Functor in an ABC Category")
                        *> pure (</>)
                    )
                    PscExpr.AssocLeft
            ]
            ]
        parserFunc :: Parser ABCCategory
        parserFunc
            = Psc.between (Psc.char '<') (Psc.char '>') parser
            Psc.<?> "Complex ABC Category"  -- with a RECURSION
        parserCommentOrEmpty :: Parser String
        parserCommentOrEmpty =
            Psc.option "" (
                Psc.char '.' 
                *> (Psc.many (Psc.letter Psc.<|> Psc.oneOf "\""))
            ) Psc.<?> "Optional Comment to an ABC Category"
        parserComplex :: Parser ABCCategory
        parserComplex
            = addComment
                <$> (parserFunc Psc.<|> parserBaseOrBot) 
                <*> parserCommentOrEmpty
                Psc.<?> "ABC Category"

-- | generate an ABC-Category from a string or a stream. 
createFromString :: String -> Either Psc.ParseError ABCCategory
createFromString 
    = Psc.parse parser "Parser of ABC Categories"