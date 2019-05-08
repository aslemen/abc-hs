{-# LANGUAGE FlexibleInstances #-}

{-|
    Module:     PTDumpable
    Copyright:  (c) T. N. Hayashi, 2019
    License:    Undetermined

    Provide an type class type of which 
    supports pretty serialization in the munge-trees style, 
    serving as an extension of the Show class in Prelude.
-}
module PTDumpable (
    -- * Types
    TreeStyle(..),
    NodeStyle(..),
    Option(..),
    -- * Classes
    Dumpable(..)
    ) where

import qualified Data.Char as DC
import qualified Data.Text as DT
import qualified Data.Text.Lazy as DTL
import qualified Data.Text.Lazy.Builder as DTLB

-- | 'TreeStyle' specifies the serialization style of trees.
data TreeStyle 
    = OneLine -- ^ Each tree is serialized in one line.
    | Pretty  -- ^ Each tree is serialized prettily in the same way of munge-trees.
    deriving (Eq, Show)

-- | 'NodeStyle' speficies the serialized style of node labels.
data NodeStyle 
    = Minimal -- ^ Each node is serialized as short as is possible.
    | Default -- ^ Each node is serialized normally.
    | Full    -- ^ Each node is serialized with full information.
    deriving (Eq, Show)

-- | 'Option' bundles all the serialization options mentioned above.
data Option 
    = Option { 
        -- | The serialization style of tree structures.
        treeStyle :: TreeStyle, 
        -- | The serialization style of nodes.
        nodeStyle :: NodeStyle
        } deriving (Eq, Show)

-- | 'Dumpable' is a class of types which support the
--      munge-trees style serialization of trees.
--      A tree is serialized with 'Data.Text.Lazy.Builder',
--          which needs further conversion to Data.Text or others.
class (Show a) => Dumpable a where
    -- | Serialize a tree with options.
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
            . psdDump (Option Pretty node)
    -- | Serialized a tree with the default option.
    --
    --      * 'PTDumpable.treeStyle': 'PTDumpable.Pretty'
    --      * 'PTDumpable.nodeStyle': 'PTDumpable.Default'
    psdDumpDefault :: a -> DTLB.Builder
    psdDumpDefault 
        = psdDump (Option Pretty Default)

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