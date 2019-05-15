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

import qualified PTDumpable as PTD

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
    (Tree term)
        -> CMonR.Reader (DumpEnv term) DTextLB.Builder
-- TODO: さらなる高速化を考える：Seqを使うか、CPSを使うか。よく検討（まずは勉強）しなければならない。
dumpPrettyInternal (Node label [])
    = CMonR.asks termDumper
        >>= \dumper ->
            return $ dumper label
dumpPrettyInternal (Node label children)
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
                                        
instance (PTD.Dumpable term) => PTD.Dumpable (Tree term) where
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