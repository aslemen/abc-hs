{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
    Module:     ParsedTree
    Copyright:  (c) T. N. Hayashi, 2019
    License:    Undetermined

    Provide an representation of parsed trees, 
        reexported from Data.Tree (provided by containers).
    The parser is available at the 'ParseParser' module.
-}
module ParsedTree (
    -- * Data (Reexported from Data.Tree)
    module Data.Tree,
    -- * Functions
    getUnary, isUnary,
    justTerminal, isTerminal,
    getNearTerminal, isNearTerminal,
    filterNearTerminal, isFilterNearTerminal
    ) where

import qualified Control.Monad as CMon
import qualified Control.Monad.Reader as CMonR

import qualified Data.Maybe as DMay
import qualified Data.List as DList
import Data.Tree
import qualified Data.Text as DText
import qualified Data.Text.Lazy as DTextL
import qualified Data.Text.Lazy.Builder as DTextLB

import qualified Data.Text.Prettyprint.Doc as PDoc

-- ## Function
getUnary :: (Tree term) -> Maybe (Tree term)
getUnary Node { subForest = child:[] } = Just child
getUnary _ = Nothing

isUnary :: (Tree term) -> Bool
isUnary = DMay.isJust . getUnary

justTerminal :: (Tree term) -> Maybe term
justTerminal (Node lex []) = Just lex
justTerminal _ = Nothing 

isTerminal :: (Tree term) -> Bool
isTerminal = DMay.isJust . justTerminal

getNearTerminal :: (Tree term) -> Maybe term
getNearTerminal = getUnary CMon.>=> justTerminal 

isNearTerminal :: (Tree term) -> Bool
isNearTerminal = DMay.isJust . getNearTerminal

filterNearTerminal :: (term -> Bool) -> (Tree term) -> Maybe term
filterNearTerminal cond tree = CMon.mfilter cond $ getNearTerminal tree

isFilterNearTerminal :: (term -> Bool) -> (Tree term) -> Bool
isFilterNearTerminal cond = DMay.isJust . (filterNearTerminal cond)

{--
    ======
    Dumping with Text-builders
    ======
--}
instance (PDoc.Pretty term) => PDoc.Pretty (Tree term) where
    pretty (Node label []) = PDoc.pretty label
    pretty (Node label children)
        = PDoc.parens 
            $ (PDoc.pretty label)
                PDoc.<+> (PDoc.align $ PDoc.vsep $ PDoc.pretty <$> children)