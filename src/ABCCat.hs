{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}

{-|
    Module:     ABCCat
    Copyright:  (c) T. N. Hayashi, 2019 - 2020
    License:    Undetermined

    Provide an representation of categories of the ABC Grammar.
-}
module ABCCat (
    -- * Types
    ABCCat(.., (:\:), (:/:))
    , ABCStatusFC(..)
    -- * Patterns
    -- * Constants
    , strBot
    -- * Functions
    , (<^#>)
    , (<^>)
    ) where

import Data.Char (isSpace)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty(..), angles, layoutCompact)
import Data.Text.Prettyprint.Doc.Render.String (renderString)
import Text.Megaparsec
import Control.Monad.Combinators.Expr (makeExprParser, Operator(InfixL))
import Data.Tree.Parser.Penn.Megaparsec.Char (
    UnsafelyParsableAsTerm(..)
    )

-- | 'ABCCat' represents ABC categories.
data ABCCat =
    -- | The Bottom ⊥.
      Bottom 
    -- | An atomic catetory.
    | BaseCategory {
        name :: Text -- ^ The name of the category.
        }
    -- | A functor category @<X\\C>@ which takes an @X@ to its left as its argument.
    | LeftFunctor { 
        antecedent :: ABCCat, -- ^ The argument, corresponding to the @X@ above.
        consequence :: ABCCat -- ^ The base, corresponding to the @C@ above.
        }
    -- | A functor category @<C/X>@ which takes an X to its right as its argument.
    | RightFunctor { 
        antecedent :: ABCCat,  -- ^ The argument, corresponding to the @X@ above.
        consequence :: ABCCat -- ^ The base, corresponding to the @C@ above.
        }

-- | The text representation of the bottom.
strBot :: Text
{-# INLINE strBot #-}
strBot = "⊥"

{-|
    Compose two categories to form a left functor category.

    Example:

    >>> (BaseCategory "NP") :\: (BaseCategory "S")
    LeftFunctor (BaseCategory "NP") (BaseCategory "S")
-}
pattern ant :\: conseq = LeftFunctor ant conseq
infixl 9 :\:

{-|
    Compose two categories to form a right functor category.

    Example:

    >>> (BaseCategory "VP") :/: (BaseCategory "NP")
    RightFunctor (BaseCategory "NP") (BaseCategory "VP")
-}
pattern conseq :/: ant  = RightFunctor ant conseq
infixl 9 :/:

instance Eq ABCCat where
    -- | The equation of ABCCategories ignores comments.
    Bottom == Bottom 
        = True
    BaseCategory x == BaseCategory y 
        = x == y
    ant1 :\: conseq1 == ant2 :\: conseq2
        = (ant1 == ant2) && (conseq1 == conseq2)
    conseq1 :/: ant1 == conseq2 :/: ant2
        = (ant1 == ant2) && (conseq1 == conseq2)
    _ == _ 
        = False

instance Pretty ABCCat where
    pretty Bottom               = pretty strBot
    pretty (BaseCategory name)  = pretty name
    pretty (ant :\: conseq)
        = angles $
            pretty ant
                <> "\\"
                <> pretty conseq
    pretty (conseq :/: ant)
        = angles $
            pretty conseq
                <> "/"
                <> pretty ant

instance Show ABCCat where
    show = renderString 
            . layoutCompact
            . pretty

instance {-# OVERLAPS #-} UnsafelyParsableAsTerm Text ABCCat where
    pUnsafeNonTerm = makeExprParser pCat opTable <?> "ABC Category"
        where
            pCat :: ParsecT _ Text m ABCCat
            pCat = between (single '<') (single '>') pUnsafeNonTerm
                <|> pBaseOrBot
            pBaseOrBot :: ParsecT _ Text m ABCCat
            pBaseOrBot = do 
                text <- takeWhile1P (Just "Atomic ABC Category") $ \c ->  
                            c /= '('
                            && c /= ')'
                            && c /= '#'
                            && c /= '<'
                            && c /= '>'
                            && not (isSpace c)
                return $ if text == strBot 
                        then Bottom 
                        else BaseCategory text
            opTable :: [[Operator _ _]]
            opTable 
                = [
                    [
                        InfixL $ single '\\' *> pure (:\:)
                    ]
                    , [
                        InfixL $ single '/' *> pure (:/:)
                    ]
                ]
    pUnsafeTerm = undefined

-------------------------------

{-|
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
    FCLeft n == FCLeft m    = n == m
    FCRight n == FCRight m  = n == m
    Failed == Failed        = True
    _ == _                  = False

instance Show ABCStatusFC where
    show (FCLeft n)
        | n > 0     = "FCLeft" <> show n
        | otherwise = "L"
    show (FCRight n)
        | n > 0     = "FCRight" <> show n
        | otherwise = "R"
    show Failed     = "FAIL"

instance Pretty ABCStatusFC

instance Semigroup ABCStatusFC where
    FCLeft i  <> FCLeft j   = FCLeft (i + j)
    FCRight i <> FCRight j  = FCRight (i + j)
    _ <> _                  = Failed

instance Monoid ABCStatusFC where
    mempty = Failed

{-| 
    Taking two ABC categories, try a reduction from them, 
        leaving the trace of the used rule.
-}
(<^#>) :: 
    ABCCat      -- ^ The category to the left.
    -> ABCCat   -- ^ The category to the right.
    -> (ABCStatusFC, ABCCat)    -- ^ The result.
base@(BaseCategory _) <^#> LeftFunctor ant2 conseq2
    | base == ant2 = (FCLeft 0, conseq2)
    | otherwise    = (Failed, Bottom)
RightFunctor ant1 conseq1 <^#> base@(BaseCategory _)
    | ant1 == base = (FCRight 0, conseq1)
    | otherwise    = (Failed, Bottom)
left@(LeftFunctor ant1 conseq1) <^#> right@(LeftFunctor ant2 conseq2)
    | left == ant2 = (FCLeft 0, conseq2)
    | otherwise    
        = let (dRes, dCat) = conseq1 <^#> right
          in case dRes of
              FCLeft n -> (FCLeft (n + 1), ant1 :\: dCat)
              _        -> (Failed, Bottom)
left@(RightFunctor ant1 conseq1) <^#> right@(RightFunctor ant2 conseq2)
    | ant1 == right = (FCRight 0, conseq1)
    | otherwise
        = let (dRes, dCat) = left <^#> conseq2
          in case dRes of 
            FCRight n -> (FCRight (n + 1), dCat :/: ant2)
            _         -> (Failed, Bottom)
left@(RightFunctor ant1 conseq1) <^#> right@(LeftFunctor ant2 conseq2) 
            -- conseq1/ant1 ant2\conseq2
    | left == ant2  = (FCLeft 0, conseq2)
    | ant1 == right = (FCRight 0, conseq1) 
    | otherwise     = (Failed, Bottom)
_ <^#> _ = (Failed, Bottom)

infix 6 <^#>

{-|
    Taking two ABC categories, try a reduction from them.
-}
(<^>) :: 
    ABCCat     -- ^ The category to the left.
    -> ABCCat  -- ^ The category to the right.
    -> ABCCat  -- ^ The result.
cat1 <^> cat2 = snd $ cat1 <^#> cat2
infixr 6 <^>