{-# LANGUAGE OverloadedStrings #-}

{-|
    Module:     KaiCat
    Copyright:  (c) T. N. Hayashi, 2019
    License:    Undetermined

    Provide an representation of categories of the Kainoki Treebank.
    The parser is available at the 'KaiCat.Parser' module.
-}

module KaiCat (
    -- * Types
    KaiCat(..),
    -- * Constructors
    createBase
    ) where

import qualified Data.Maybe as DM
import qualified Data.Char as DCh
import qualified Data.Text as DT
import qualified Data.Text.Lazy as DTL
import qualified Data.Text.Lazy.Builder as DTLB
import qualified Data.String as DS

import qualified PTDumpable as PTD

-- | 'KaiCat' represents Kainoki categories.
data KaiCat =
    -- | A Kainoki category.
    KaiCat {
        catlist :: [DT.Text], -- ^ The category name, separated by hyphens.
        iched :: Int, -- ^ The ICH index. 0 for non-indexed cases.
        sortInfo :: DT.Text -- ^ The sort information. Empty when none.
    }
    deriving (Eq)

-- | Construct a non-indexed, non-sort-infoed Kainoki category.
createBase :: 
    [DT.Text] -- ^ A split series of category.
    -> KaiCat -- ^ The resulted representation.
createBase strlist
    = KaiCat {
        catlist = strlist,
        iched = 0,
        sortInfo = ""
    }

isToBeEscaped :: DT.Text -> Bool
isToBeEscaped
    = DT.any (\c -> DCh.isSpace c || c `elem` ("()-'" :: [Char]))

dumpCatList :: KaiCat -> DTLB.Builder
dumpCatList (KaiCat catList _ _)
    = DTLB.fromText $ DT.intercalate "-" $ escapedCat <$> catList
    where 
        escapedCat :: DT.Text -> DT.Text
        escapedCat str
            = if isToBeEscaped str
                then "{" <> str <> "}"
                else str

dumpCat :: KaiCat -> DTLB.Builder
dumpCat cat@(KaiCat _ i sort)
    = dumpCatList cat
        <> (
            if i /= 0 
                then (DTLB.singleton '-' <> (DTLB.fromString $ show i))
                else mempty
            )
        <> (
            if sort /= ""
                then (
                    DTLB.fromText ";{" 
                    <> DTLB.fromText sort 
                    <> DTLB.singleton '}'
                    )
                else mempty
        )

instance PTD.Dumpable KaiCat where
    psdDump (PTD.Option _ PTD.Minimal) 
        = dumpCatList
    psdDump _ 
        = dumpCat

instance Show KaiCat where
    show = DTL.unpack . DTLB.toLazyText . PTD.psdDumpDefault