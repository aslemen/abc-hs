{-# LANGUAGE OverloadedStrings #-}

{-|
    Module:     ABCCategory
    Copyright:  (c) T. N. Hayashi, 2019
    License:    Undetermined

    Provide an representation of categories of the ABC Grammar.
    The parser is available at the 'ABCCategory.Parser' module.
-}
module ABCCategory (
    -- * Types
    ABCCategory(..),
    -- ** Subsidiaries
    Com.ABCComment(..),
    ABCCategoryCommented(..),
    ABCStatusFC(..),
    -- * Constants
    strBot,
    -- * Functions
    -- ** For Composition
    (</>),
    makeLeftAdjunct,
    (<\>),
    makeRightAdjunct,
    -- ** For Reduction
    (<^>),
    reduceWithResult,
    reduceWithLog,
    ) where

import Control.Applicative

import Data.Maybe as DM
import Data.Char as DCh
import Data.Text as DT
import Data.Text.Lazy as DTL
import Data.Text.Lazy.Builder as DTLB
import Data.String as DS

import qualified PTPrintable as PTP
import qualified PTDumpable as PTD

import qualified ABCComment as Com

-- | 'ABCCategory' represents ABC categories.
data ABCCategory =
    -- | The Bottom ⊥.
      Bottom 
    -- | An atomic catetory.
    | BaseCategory {
        name :: DT.Text -- ^ The name of the category.
        }
    -- | A functor category @<X\\C>@ which takes an @X@ to its left as its argument.
    | LeftFunctor { 
        antecedent :: ABCCategory, -- ^ The argument, corresponding to the @X@ above.
        consequence :: ABCCategory -- ^ The base, corresponding to the @C@ above.
        }
    -- | A functor category @<C/X>@ which takes an X to its right as its argument.
    | RightFunctor { 
        antecedent :: ABCCategory,  -- ^ The argument, corresponding to the @X@ above.
        consequence :: ABCCategory -- ^ The base, corresponding to the @C@ above.
        }

-- delete it!
type ABCCategoryCommented = Com.ABCComment ABCCategory

-- | The text representation of the bottom.
strBot :: DT.Text
strBot = "⊥"

-- | Compose two categories to form a left functor category.
-- 
--   Example:
-- 
--   >>> (BaseCategory "NP") <\> (BaseCategory "S")
--   LeftFunctor (BaseCategory "NP") (BaseCategory "S")
(<\>) :: 
    ABCCategory -- ^ The argument category @X@.
    -> ABCCategory -- ^ The base category @C@
    -> ABCCategory -- ^ The resulted category @<X\\C>@. 
ant <\> conseq 
    = LeftFunctor {
        antecedent = ant,
        consequence = conseq
        }

makeLeftAdjunct :: ABCCategory -> ABCCategory
makeLeftAdjunct c = c <\> c

-- | Compose two categories to form a right functor category.
-- 
--   Example:
-- 
--   >>> (BaseCategory "VP") </> (BaseCategory "NP")
--   RightFunctor (BaseCategory "NP") (BaseCategory "VP")
(</>) :: 
    ABCCategory -- ^ The base category @C@
    -> ABCCategory -- ^ The argument category @X@.
    -> ABCCategory -- ^ The resulted category @<C/X>@. 
conseq </> ant 
    = RightFunctor {
        antecedent = ant, 
        consequence = conseq
        }

makeRightAdjunct :: ABCCategory -> ABCCategory
makeRightAdjunct c = c </> c

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
instance PTD.Dumpable ABCCategory where
    psdDump _ Bottom 
        = DTLB.fromText strBot
    psdDump _ (BaseCategory name) 
        = DTLB.fromText name
    psdDump opt (LeftFunctor ant conseq)
        = (DTLB.singleton '<')
            <> (PTD.psdDump opt ant)
            <> (DTLB.singleton '\\')
            <> (PTD.psdDump opt conseq)
            <> (DTLB.singleton '>')
    psdDump opt (RightFunctor ant conseq)
        = (DTLB.singleton '<')
            <> (PTD.psdDump opt conseq)
            <> (DTLB.singleton '/')
            <> (PTD.psdDump opt ant)
            <> (DTLB.singleton '>')

instance Show ABCCategory where
    show = DTL.unpack . DTLB.toLazyText . PTD.psdDumpDefault

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

instance PTD.Dumpable ABCStatusFC where
    psdDump _ (FCLeft n)
        | n > 0 
            = (DTLB.fromText "FCLeft")
                <> (DTLB.fromString $ show n)
        | otherwise
            = DTLB.singleton 'L'
    psdDump _ (FCRight n)
        | n > 0
            = (DTLB.fromText "FCRight")
                <> (DTLB.fromString $ show n)
        | otherwise
            = DTLB.singleton 'R'
    psdDump _ Failed
        = DTLB.fromText "FAIL"
instance Show ABCStatusFC where
    show = DTL.unpack . DTLB.toLazyText . PTD.psdDumpDefault 


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
    = Com.ABCComment {
        Com.content 
            = cat, 
        Com.comment 
            = DTL.toStrict 
                $ DTLB.toLazyText 
                $ PTD.psdDumpDefault res 
        }
        where
            cat :: ABCCategory
            res :: ABCStatusFC
            (cat, res) = reduceWithResult left right

(<^>) :: ABCCategory -> ABCCategory -> ABCCategory
cat1 <^> cat2 
    = fst (reduceWithResult cat1 cat2)