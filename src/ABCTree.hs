{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
    Module:     ABCTree
    Copyright:  (c) T. N. Hayashi, 2020
    License:    Undetermined
    Provide utilities for ABC Trees.
-}
module ABCTree (
    -- * Data Types
    ABCTree
    -- * Printers
    -- ** Printer options
    , BranchPrintOption(..)
    , NodePrintOption(..)
    , PrintOption(..)
    -- ** Printer functions
    , printABCTree
    -- * Data (Reexported from Data.Tree)
    , module Data.Tree
    ) where

import Data.Tree
import qualified Data.Text.Prettyprint.Doc as PDoc

import ABCCat
import CatPlus

type ABCTree = Tree (CatPlus ABCCat)

data BranchPrintOption = Indented | OneLine deriving (Eq, Show)
data NodePrintOption
    = Full      -- | Print all categories and feature-value annotations
    | Normal    -- | Print all information but self-evident derivational rules
    | Compact   -- | Omit self-evident derivational and grammatical roles
    | Minimal   -- | Omit even categories that are predicatable
    deriving (Eq, Show)

getCatPlusPrintOption :: NodePrintOption -> CatPlusPrintOption
getCatPlusPrintOption Full = CatPlusPrintOption False False False
getCatPlusPrintOption Normal = CatPlusPrintOption {
    omitCat = False
    , omitSelfEvidentDeriv = True
    , omitRole = False
}
getCatPlusPrintOption _ = CatPlusPrintOption False True True

data PrintOption = PrintOption {
    branchOption :: BranchPrintOption
    , nodeOption :: NodePrintOption
} deriving (Eq, Show)

printABCTree :: PrintOption -> ABCTree -> PDoc.Doc ann
printABCTree (PrintOption Indented _) (Node label [])
    = PDoc.group $ PDoc.pretty label
printABCTree pOp@(PrintOption Indented nOp) (Node label children) 
    = PDoc.parens pInner
    where 
        isNodeOmittable :: Bool
        isNodeOmittable = case label of 
            NonTerm { deriv = d } -> case d of
                LeftApp _    -> True
                RightApp _   -> True
                otherwise -> False
            otherwise -> False
        cpOption :: CatPlusPrintOption
        cpOption = getCatPlusPrintOption nOp
        finalOption :: CatPlusPrintOption
        finalOption 
            = cpOption {
                omitCat = (omitCat cpOption) && isNodeOmittable 
            } 
        pInner 
            = (printCatPlus finalOption label) 
            PDoc.<+> (PDoc.align $ PDoc.vsep $ printABCTree pOp <$> children)
printABCTree pO@(PrintOption OneLine _) tr
    = PDoc.group $ printABCTree pO {branchOption = Indented} tr
