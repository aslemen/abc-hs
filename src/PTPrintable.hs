{-# LANGUAGE FlexibleInstances #-}

module PTPrintable (
    PTD.TreeStyle(..),
    PTD.NodeStyle(..),
    PTD.Option(..),
    Printable(..)
    ) where

import qualified PTDumpable as PTD

type TreeStyle = PTD.TreeStyle
type NodeStyle = PTD.NodeStyle
type Option = PTD.Option

class (Show a) => Printable a where
    psdPrint :: Option -> a -> String
    psdPrint (PTD.Option PTD.Pretty PTD.Default)  -- 普段遣い
        = show -- defaultではshowと同じ
    psdPrint (PTD.Option PTD.Pretty _) 
        = psdPrint (PTD.Option PTD.Pretty PTD.Default)
    psdPrint (PTD.Option PTD.OneLine node) -- oneLine
        = filter (\c -> c /= '\n') . psdPrint (PTD.Option PTD.Pretty PTD.Default) 
    
    psdPrintDefault :: a -> String
    psdPrintDefault = psdPrint (PTD.Option PTD.Pretty PTD.Default) 
        -- default print style

instance Printable String where
    psdPrint (PTD.Option PTD.Pretty PTD.Default) = id