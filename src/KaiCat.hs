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

import qualified Data.Text.Prettyprint.Doc as PDoc
import qualified Data.Text.Prettyprint.Doc.Render.String as PDocRS

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

instance PDoc.Pretty KaiCat where
    pretty cat@(KaiCat catList i sort)
        = (
            PDoc.pretty $ DT.intercalate "-" $ escapedCat <$> catList
        ) <> (
                if i /= 0 
                    then "-" <> PDoc.pretty i
                    else mempty
            )
            <> (
                if sort /= ""
                    then ";" <> (PDoc.braces $ PDoc.pretty sort) 
                    else mempty
            )
        where 
            escapedCat :: DT.Text -> DT.Text
            escapedCat str
                = if isToBeEscaped str
                    then "{" <> str <> "}"
                    else str

instance Show KaiCat where
    show = PDocRS.renderString
            . PDoc.layoutCompact
            . PDoc.pretty 