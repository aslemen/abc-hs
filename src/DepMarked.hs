{-# LANGUAGE OverloadedStrings #-}

module DepMarked (
    DepMarked(..),
    markCat,
    marker
    ) where

import Control.Applicative

import qualified Data.Text as DT
import qualified Data.Text.Lazy.Builder as DTLB

import qualified Text.Megaparsec as TMega

import qualified DepMarking as DMing
import qualified Data.Text.Prettyprint.Doc as PDoc

data DepMarked cat
    = (:|) {
        category:: cat,
        dependency :: DMing.DepMarking
    } deriving (Eq)

marker :: DT.Text
marker = "''"

instance (Show cat) => Show (DepMarked cat) where
    show (cat :| dep) = (show cat) ++ (DT.unpack marker) ++ (show dep)

instance (PDoc.Pretty cat) => PDoc.Pretty (DepMarked cat) where
    pretty (cat :| DMing.None)
        = PDoc.pretty cat
    pretty (cat :| dep)
        = PDoc.pretty cat <> PDoc.pretty marker <> (PDoc.pretty dep)

instance Functor DepMarked where
    -- fmap :: (a -> b) -> (DepMarked a) -> (DepMarked b)
    fmap f (cat :| dep) = (f cat) :| dep

markCat :: DT.Text -> cat -> DepMarked cat
markCat str x
    = x :| (DMing.createMarking str)