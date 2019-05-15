{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ParsedTree (
    DTree.Tree(..),
    DTree.Forest,
    getUnary, isUnary,
    justTerminal, isTerminal,
    getNearTerminal, isNearTerminal,
    filterNearTerminal, isFilterNearTerminal
    ) where

import qualified Control.Monad as CMon
import qualified Control.Monad.Reader as CMonR

import qualified Data.Maybe as DMay
import qualified Data.List as DList
import qualified Data.Tree as DTree
import qualified Data.Text as DText
import qualified Data.Text.Lazy as DTextL
import qualified Data.Text.Lazy.Builder as DTextLB

import qualified PTDumpable as PTD

-- ## Function
getUnary :: (DTree.Tree term) -> Maybe (DTree.Tree term)
getUnary DTree.Node { DTree.subForest = child:[] } = Just child
getUnary _ = Nothing

isUnary :: (DTree.Tree term) -> Bool
isUnary = DMay.isJust . getUnary

justTerminal :: (DTree.Tree term) -> Maybe term
justTerminal (DTree.Node lex []) = Just lex
justTerminal _ = Nothing 

isTerminal :: (DTree.Tree term) -> Bool
isTerminal = DMay.isJust . justTerminal

getNearTerminal :: (DTree.Tree term) -> Maybe term
getNearTerminal = getUnary CMon.>=> justTerminal 

isNearTerminal :: (DTree.Tree term) -> Bool
isNearTerminal = DMay.isJust . getNearTerminal

filterNearTerminal :: (term -> Bool) -> (DTree.Tree term) -> Maybe term
filterNearTerminal cond tree = CMon.mfilter cond $ getNearTerminal tree

isFilterNearTerminal :: (term -> Bool) -> (DTree.Tree term) -> Bool
isFilterNearTerminal cond = DMay.isJust . (filterNearTerminal cond)

{--
    ======
    Dumping with Text-builders
    ======
--}
data DumpEnv term
    = DumpEnv {
        termDumper :: term -> DTextLB.Builder, -- Readonly
        indent :: Int
    }
    
initEnv :: (PTD.Dumpable term) => DumpEnv term
initEnv 
    = DumpEnv {
        termDumper = PTD.psdDumpDefault,
        indent = 0
    } 

dumpPrettyInternal :: 
    (PTD.Dumpable term) => 
    (DTree.Tree term)
        -> CMonR.Reader (DumpEnv term) DTextLB.Builder
-- TODO: さらなる高速化を考える：Seqを使うか、CPSを使うか。よく検討（まずは勉強）しなければならない。
dumpPrettyInternal (DTree.Node label [])
    = CMonR.asks termDumper
        >>= \dumper ->
            return $ dumper label
dumpPrettyInternal (DTree.Node label children)
    = CMonR.ask
        >>= \env -> case env of 
            DumpEnv dumper root_indent
                -> (
                    CMonR.local 
                        (
                            \env -> env {
                                indent 
                                    = root_indent
                                        + 1
                                        + fromIntegral (
                                            -- unsafe conversion, Int64 -> Int
                                            DTextL.length 
                                            $ DTextLB.toLazyText
                                            $ dumper label
                                        )
                                        + 1
                            }
                        )
                        (
                            foldr 
                                (CMonR.liftM2 (<>)) 
                                (return $ DTextLB.fromText DText.empty)
                                $ DList.intersperse 
                                    (
                                        CMonR.asks indent
                                            >>= \ind ->
                                                return $ DTextLB.fromText
                                                    $ "\n" <> (DText.replicate ind " ")
                                    )
                                    $ map dumpPrettyInternal children
                        )
                        >>= \text_children ->
                            return (
                                DTextLB.singleton '('
                                <> dumper label
                                <> DTextLB.singleton ' '
                                <> text_children
                                <> DTextLB.singleton ')'
                            )
                )
                                        
instance (PTD.Dumpable term) => PTD.Dumpable (DTree.Tree term) where
    psdDump opt tree 
        = (
            CMonR.runReader (dumpPrettyInternal tree) env
        ) <> (DTextLB.fromText "\n\n")
        where
            env :: DumpEnv term
            env
                = DumpEnv {
                    termDumper = PTD.psdDump opt,
                    indent = 0
                    }