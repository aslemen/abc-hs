{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module PTPrintable (
    TreeStyle(..),
    NodeStyle(..),
    Option(..),
    Printable(..)
    ) where

data TreeStyle 
    = OneLine | Pretty 
        deriving (Eq, Show)
data NodeStyle 
    = Minimal | Default | Full
        deriving (Eq, Show)
data Option 
    = Option { 
        treeStyle :: TreeStyle, 
        nodeStyle :: NodeStyle
        } deriving (Eq, Show)

class (Show a) => Printable a where
    psdPrint :: Option -> a -> String
    psdPrint (Option Pretty Default)  -- 普段遣い
        = show -- defaultではshowと同じ
    psdPrint (Option Pretty _) 
        = psdPrint (Option Pretty Default)
    psdPrint (Option OneLine node) -- oneLine
        = filter (\c -> c /= '\n') . psdPrint (Option Pretty Default) 
    
    psdPrintDefault :: a -> String
    psdPrintDefault = psdPrint (Option Pretty Default) 
        -- default print style

instance Printable String where
    psdPrint (Option Pretty Default) = id