module ABCCategory (
    ABCCategory(..),
    ABCComment(..),
    ABCCategoryCommented(..),
    strBot,
    (</>),
    (<\>),
    (<^>),
    ABCStatusFC(..),
    reduceWithResult,
    reduceWithLog,
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
      Bottom
    | BaseCategory {
        name :: String
        }
    | LeftFunctor { 
        antecedent :: ABCCategory, 
        consequence :: ABCCategory
        }
    | RightFunctor { 
        antecedent :: ABCCategory, 
        consequence :: ABCCategory
        }

data ABCComment a 
    = ABCComment {
        content :: a,
        comment :: String
    }
type ABCCategoryCommented = ABCComment ABCCategory

instance (Eq a) => Eq (ABCComment a) where
    (==) (ABCComment a _) (ABCComment b _)
        = (a == b)

instance (Show a) => Show (ABCComment a) where
    show (ABCComment a c)
        = if c == ""
            then show a
            else (show a) ++ ".\"" ++ c ++ "\""
instance (PTP.Printable a) => PTP.Printable (ABCComment a) where
    psdPrint opt@(PTP.Option _ PTP.Minimal)
        = \ca -> PTP.psdPrint opt $ content ca
    psdPrint _ 
        = show

instance Functor ABCComment where
    fmap f ABCComment {
        content = a,
        comment = c
        } = ABCComment {
            content = f a,
            comment = c
        }

instance Applicative ABCComment where
    pure a
        = ABCComment a ""
    (<*>) (ABCComment f cf) (ABCComment a ca)
        = ABCComment (f a) (cf ++ ";" ++ ca)

instance Monad ABCComment where
    (>>=) (ABCComment a ca) f
        = ABCComment com_new_content (ca ++ ";" ++ com_new_comment)
            where
                -- com_new :: ABCComment sth
                com_new = f a
                -- com_new_content :: sth
                com_new_content = content com_new
                com_new_comment :: String
                com_new_comment = comment com_new

-- instance (Monoid w) => CMW.MonadWriter w ABCComment where
--    writer (a, w)
--        = ABCComment {
--            content = a,
--            comment = w
--        }
--    tell w
--        = ABCComment {
--            content = (),
--            comment = w
--        }
--    listen ABCComment {
--        content = a,
--        comment = c
--        } = ABCComment {
--            content = (a, c),
--            comment = c
--        }
--    pass ABCComment{
--        content = (a, f),
--        comment = c
--        } = ABCComment{
--            content = a,
--            comment = f c
--        }
        
strBot :: String
strBot = "⊥"

(<\>) :: ABCCategory -> ABCCategory -> ABCCategory
ant <\> conseq 
    = LeftFunctor {
        antecedent = ant,
        consequence = conseq
        }

(</>) :: ABCCategory -> ABCCategory -> ABCCategory
conseq </> ant 
    = RightFunctor {
        antecedent = ant, 
        consequence = conseq
        }

-- ## Equation
instance Eq ABCCategory where
    -- | The equation of ABCCategories ignores comments.
    (==) Bottom Bottom 
        = True
    (==) (BaseCategory x) (BaseCategory y) 
        = (x == y)
    (==) (LeftFunctor ant1 conseq1) (LeftFunctor ant2 conseq2)
        = (ant1 == ant2) && (conseq1 == conseq2)
    (==) (RightFunctor ant1 conseq1) (RightFunctor ant2 conseq2)
        = (ant1 == ant2) && (conseq1 == conseq2)
    (==) _ _ 
        = False

-- ## Showing

-- | Provide a string representation of an ABCCategory.
instance Show ABCCategory where
    show Bottom 
        = strBot
    show (BaseCategory name) 
        = name
    show (LeftFunctor ant conseq)
        = "<" ++ (show ant) ++ "\\" ++ (show conseq) ++ ">"
    show (RightFunctor ant conseq)
        = "<" ++ (show conseq) ++ "/" ++ (show ant) ++ ">" 

instance PTP.Printable ABCCategory where
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
catStatFailed = (Bottom, Failed)

reduceWithResult :: ABCCategory -> ABCCategory -> (ABCCategory, ABCStatusFC)
reduceWithResult base@(BaseCategory _) (LeftFunctor ant2 conseq2)
    | base == ant2
        = (conseq2, FCLeft 0)
    | otherwise
        = catStatFailed
reduceWithResult (RightFunctor ant1 conseq1) base@(BaseCategory _)
    | ant1 == base
        = (conseq1, FCRight 0)
    | otherwise
        = catStatFailed
reduceWithResult 
    left@(LeftFunctor ant1 conseq1)
    right@(LeftFunctor ant2 conseq2)
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
    left@(RightFunctor ant1 conseq1)
    right@(RightFunctor ant2 conseq2)
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
    left@(RightFunctor ant1 conseq1)
    right@(LeftFunctor ant2 conseq2) -- conseq1/ant1 ant2\conseq2
    | left == ant2
        = (conseq2, FCLeft 0)
    | ant1 == right
        = (conseq1, FCRight 0)
    | otherwise
        = catStatFailed
reduceWithResult _ _ 
    = catStatFailed

reduceWithLog :: ABCCategory -> ABCCategory -> ABCCategoryCommented
reduceWithLog left right
    = ABCComment { content = cat, comment = show res }
        where
            cat :: ABCCategory
            res :: ABCStatusFC
            (cat, res) = reduceWithResult left right

(<^>) :: ABCCategory -> ABCCategory -> ABCCategory
cat1 <^> cat2 
    = fst (reduceWithResult cat1 cat2)

-- ## Parsing
parser :: Parser ABCCategoryCommented
parser
    = (
        ABCComment <$> parserSimplex <*> parserCommentOrEmpty
    ) Psc.<?> "ABC Category"
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
                        = Bottom
                    | otherwise
                        = BaseCategory str
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
            = Psc.between (Psc.char '<') (Psc.char '>') parserCat
            Psc.<?> "Complex ABC Category"  -- with a RECURSION
        parserCat :: Parser ABCCategory
        parserCat = parserFunc Psc.<|> parserBaseOrBot
        parserSimplex :: Parser ABCCategory
        parserSimplex = PscExpr.buildExpressionParser opTableComplex parserCat
        parserCommentOrEmpty :: Parser String
        parserCommentOrEmpty =
            Psc.option "" (
                Psc.char '.' 
                *> (Psc.many (Psc.letter Psc.<|> Psc.oneOf "\""))
            ) Psc.<?> "Optional Comment to an ABC Category"

-- | generate an ABC-Category from a string or a stream. 
createFromString :: String -> Either Psc.ParseError ABCCategoryCommented
createFromString 
    = Psc.parse parser "Parser of ABC Categories"