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

import qualified Control.Monad.State as CMS

import qualified Data.Maybe as DM
import qualified Data.Char as DCh
import qualified Data.Text as DT
import qualified Data.Text.Lazy as DTL
import qualified Data.Text.Lazy.Builder as DTLB
import qualified Data.String as DS

import qualified Data.Text.Prettyprint.Doc as PDoc
import qualified Data.Text.Prettyprint.Doc.Render.String as PDocRS
import qualified Data.Text.Prettyprint.Doc.Render.Text as PDocRT

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

-- | The text representation of the bottom.
strBot :: DT.Text
strBot = "⊥"

{-|
    Compose two categories to form a left functor category.

    Example:

    >>> (BaseCategory "NP") <\> (BaseCategory "S")
    LeftFunctor (BaseCategory "NP") (BaseCategory "S")
-}
(<\>) :: 
    ABCCategory -- ^ The argument category @X@.
    -> ABCCategory -- ^ The base category @C@
    -> ABCCategory -- ^ The resulted category @<X\\C>@. 
ant <\> conseq 
    = LeftFunctor {
        antecedent = ant,
        consequence = conseq
        }

{-|
    Make an endomorphic adjunct @<X\\X>@ from a category @X@.

    Example:
    
    >>> makeLeftAdjunct (BaseCategory "S")
    LeftFunctor (BaseCategory "S") (BaseCategory "S")
-}
makeLeftAdjunct :: 
    ABCCategory     -- ^ The argument category @X@.
    -> ABCCategory  -- ^ The resulted category @<X\\X>@.
makeLeftAdjunct c = c <\> c

{-|
    Compose two categories to form a right functor category.

    Example:

    >>> (BaseCategory "VP") </> (BaseCategory "NP")
    RightFunctor (BaseCategory "NP") (BaseCategory "VP")
-}
(</>) :: 
    ABCCategory -- ^ The base category @C@
    -> ABCCategory -- ^ The argument category @X@.
    -> ABCCategory -- ^ The resulted category @<C/X>@. 
conseq </> ant 
    = RightFunctor {
        antecedent = ant, 
        consequence = conseq
        }

{-|
    Make an endomorphic adjunct @<X/X>@ from a category @X@.

    Example:
    
    >>> makeRightAdjunct (BaseCategory "S")
    RightFunctor (BaseCategory "S") (BaseCategory "S")
-}
makeRightAdjunct :: 
    ABCCategory      -- ^ The argument category @X@.
    -> ABCCategory   -- ^ The resulted category @<X/X>@.
makeRightAdjunct c = c </> c

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

instance PDoc.Pretty ABCCategory where
    pretty Bottom =  PDoc.pretty strBot
    pretty (BaseCategory name) = PDoc.pretty name
    pretty (LeftFunctor ant conseq)
        = PDoc.angles $
            PDoc.pretty ant
                <> "\\"
                <> (PDoc.pretty conseq)
    pretty (RightFunctor ant conseq)
        = PDoc.angles $
            (PDoc.pretty conseq)
                <> "/"
                <> (PDoc.pretty ant)

instance Show ABCCategory where
    show = PDocRS.renderString 
            . PDoc.layoutCompact
            . PDoc.pretty
{-
    'ABCStatusFC' represents the rule which has been used 
        in a process of reduction.
-}
data ABCStatusFC = 
        {-| 
            Indicating that
                a left reduction/functional-composition rule is used.
        -}
          FCLeft Int
        {-| 
            Indicating that
                a right reduction/functional-composition rule is used.
        -}
        | FCRight Int
        {-|
            Indicating that the reduction cannot be achieved.
        -}
        | Failed

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

instance PDoc.Pretty ABCStatusFC where
    pretty (FCLeft n)
        | n > 0 
            = "FCLeft" <> PDoc.pretty n
        | otherwise
            = "L"
    pretty (FCRight n)
        | n > 0
            = "FCRight" <> PDoc.pretty n
        | otherwise
            = "R"
    pretty Failed
        = "FAIL"

instance Show ABCStatusFC where
    show = PDocRS.renderString 
            . PDoc.layoutCompact
            . PDoc.pretty

instance Semigroup ABCStatusFC where
    FCLeft i <> FCLeft j = FCLeft (i + j)
    FCRight i <> FCRight j = FCRight (i + j)
    _ <> _ = Failed

instance Monoid ABCStatusFC where
    mempty = Failed

{-| 
    Taking two ABC categories, try a reduction from them, 
        leaving the trace of the used rule.
-}
reduceWithResult :: 
    ABCCategory      -- ^ The category to the left.
    -> ABCCategory   -- ^ The category to the right.
    -> CMS.State ABCStatusFC ABCCategory    -- ^ The result.
reduceWithResult base@(BaseCategory _) (LeftFunctor ant2 conseq2)
    | base == ant2
        = (CMS.put $ FCLeft 0)
            >> return conseq2
    | otherwise
        = (CMS.put $ Failed) 
            >> return Bottom
reduceWithResult (RightFunctor ant1 conseq1) base@(BaseCategory _)
    | ant1 == base
        = (CMS.put $ FCRight 0)
            >> return conseq1
    | otherwise
        = (CMS.put $ Failed) 
            >> return Bottom
reduceWithResult 
    left@(LeftFunctor ant1 conseq1)
    right@(LeftFunctor ant2 conseq2)
    | left == ant2
        = (CMS.put $ FCLeft 0)
            >> return conseq2
    | otherwise
        = reduceWithResult conseq1 right
            >>= \dcat ->
                CMS.get
                >>= \dres ->
                    case dres of
                        FCLeft n
                            -> (CMS.put $ FCLeft (n + 1))
                                    >> (return $ ant1 <\> dcat)
                        _
                            -> (CMS.put $ Failed) 
                                >> return Bottom
reduceWithResult 
    left@(RightFunctor ant1 conseq1)
    right@(RightFunctor ant2 conseq2)
    | ant1 == right
        = (CMS.put $ FCRight 0)
            >> return conseq1
    | otherwise
        = reduceWithResult left conseq2
            >>= \dcat ->
                CMS.get
                >>= \dres ->
                    case dres of
                        FCRight n
                            -> (CMS.put $ FCRight (n + 1))
                                >> (return $ dcat </> ant2)
                        _
                            -> (CMS.put $ Failed) 
                                >> return Bottom
reduceWithResult 
    left@(RightFunctor ant1 conseq1)
    right@(LeftFunctor ant2 conseq2) -- conseq1/ant1 ant2\conseq2
    | left == ant2
        = (CMS.put $ FCLeft 0)
            >> return conseq2
    | ant1 == right
        = (CMS.put $ FCRight 0)
            >> return conseq1
    | otherwise
        = (CMS.put $ Failed) 
            >> return Bottom
reduceWithResult _ _ 
    = (CMS.put $ Failed) 
        >> return Bottom

reduceWithLog :: ABCCategory -> ABCCategory -> Com.ABCComment ABCCategory
reduceWithLog left right
    = Com.ABCComment {
        Com.content 
            = cat
        ,
        Com.comment 
            = (PDocRT.renderStrict
                $ PDoc.layoutCompact
                $ PDoc.pretty res )
        }
        where
            cat :: ABCCategory
            res :: ABCStatusFC
            (cat, res) = CMS.runState (reduceWithResult left right) Failed 

{-|
    Taking two ABC categories, try a reduction from them.
-}
(<^>) :: 
    ABCCategory     -- ^ The category to the left.
    -> ABCCategory  -- ^ The category to the right.
    -> ABCCategory  -- ^ The result.
cat1 <^> cat2 
    = (reduceWithResult cat1 cat2) `CMS.evalState` Failed