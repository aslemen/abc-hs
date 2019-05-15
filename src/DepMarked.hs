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
import qualified PTDumpable as PTD

data DepMarked cat
    = (:|) {
        category:: cat,
        dependency :: DMing.DepMarking
    } deriving (Eq)

marker :: DT.Text
marker = "''"

instance (Show cat) => Show (DepMarked cat) where
    show (cat :| dep) = (show cat) ++ (DT.unpack marker) ++ (show dep)

instance (PTD.Dumpable cat) => PTD.Dumpable (DepMarked cat) where
    psdDump
        opt@(PTD.Option _ optNode)
        dm@(cat :| DMing.None)
            | optNode == PTD.Full
                = (PTD.psdDump opt cat) 
                    <> (DTLB.fromText marker)
                    <> (PTD.psdDump opt DMing.None)
            | otherwise 
                = PTD.psdDump opt cat
    psdDump opt dm@(cat :| dep)
        = (PTD.psdDump opt cat) 
            <> (DTLB.fromText marker)
            <> (PTD.psdDump opt dep)
    
instance Functor DepMarked where
    -- fmap :: (a -> b) -> (DepMarked a) -> (DepMarked b)
    fmap f (cat :| dep) = (f cat) :| dep

markCat :: DT.Text -> cat -> DepMarked cat
markCat str x
    = x :| (DMing.createMarking str)