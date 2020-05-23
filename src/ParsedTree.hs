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
    ) where

import Data.Tree
import qualified Data.Text.Prettyprint.Doc as PDoc

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