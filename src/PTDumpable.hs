{-# LANGUAGE FlexibleInstances #-}

module PTDumpable (
    TreeStyle(..),
    NodeStyle(..),
    Option(..),
    Dumpable(..)
    ) where

import qualified Data.Char as DC
import qualified Data.Text as DT
import qualified Data.Text.Lazy as DTL
import qualified Data.Text.Lazy.Builder as DTLB

{-- 
    ======
    Data Types
    ======
--}
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

{--
    ======
    CLass Definition
    ======
--}
class (Show a) => Dumpable a where
    psdDump :: Option -> a -> DTLB.Builder
    psdDump (Option Pretty Default)  -- 普段遣い
        = DTLB.fromString . show 
            -- defaultではshowと同じだが、もっと効率のよいものがあっても構わない
    psdDump (Option Pretty _) 
        = psdDump (Option Pretty Default)
            -- defaultではPretty Defaultと同じ
    psdDump (Option OneLine node) -- oneLine
        = DTLB.fromLazyText
            . (DTL.filter (\c -> c /= '\n')) 
            . DTLB.toLazyText 
            . psdDump (Option Pretty node) -- defaultではやはりshow
    psdDumpDefault :: a -> DTLB.Builder
    psdDumpDefault 
        = psdDump (Option Pretty Default) 
        -- default print style

instance Dumpable String

instance Dumpable DT.Text where
    psdDump (Option Pretty _)
        = DTLB.fromText
    psdDump (Option OneLine node)
        = (psdDump (Option Pretty node))
            . (DT.filter (\c -> c /= '\n')) 

instance Dumpable DTL.Text where
    psdDump (Option Pretty _)
        = DTLB.fromLazyText
    psdDump (Option OneLine node)
        = (psdDump (Option Pretty node))
            . (DTL.filter (\c -> c /= '\n')) 

instance Dumpable DTLB.Builder where
    psdDump (Option Pretty _) = id