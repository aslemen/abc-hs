{- 
The Pipeline:
- pretreatments
- head-marking (Haskell? tsurgeon?)
- **relabelling**
-}

module Relabeling where

import Data.Maybe

import qualified KeyakiCategory as KC
import qualified DepMarking as DMing
import qualified DepMarked as DMed
import qualified ABCCategory as ABCC

import qualified ParsedTree as PT
import qualified PTPrintable as PTP

-- # Type Aliases
type KCat = KC.KeyakiCategory
type ABCCat = ABCC.ABCCategory

type KCatMarked = DMed.DepMarked KCat
type ABCCatMarked = DMed.DepMarked ABCCat

type KTMarked = PT.Tree KCatMarked
type ABCTMarked = PT.Tree ABCCatMarked


parserKTMarked = DMed.parser KC.parser

-- # Tree Parser
runParserKTMarked :: String -> Either PT.ParseError KTMarked
runParserKTMarked
    = PT.createFromString parserKTMarked

runParserDoc :: String -> Either PT.ParseError [KTMarked]
runParserDoc = PT.createDoc parserKTMarked

-- # Main Job
createABCCBaseFromKC :: KCat -> ABCCat
createABCCBaseFromKC 
    = ABCC.createBase 
        . (PTP.psdPrint (PTP.Option PTP.Pretty PTP.Minimal))

relabel :: KTMarked -> ABCTMarked
relabel node@(PT.Node (cat DMed.:| _) _)
    = snd $ relabelLoopLeft (createABCCBaseFromKC cat) node 
    where
        -- | relabelLoopLeft 
        --      givenCatParent isHeaded oldTree ~~> (isHeaded, New Tree)
        relabelLoopLeft :: ABCCat -> KTMarked -> (Bool, ABCTMarked)
        relabelLoopLeft 
            givenCatParent -- 上・左から降ってくる新しい親範疇
            oldTree@( -- もとのtree :: KTMarked
                PT.Node rootLabel (
                    treeLeftMost@(
                        PT.Node { 
                            PT.rootLabel = catLeftMost DMed.:|depLeftMost 
                            }
                        ):remainder -- leftmost childを取ってくる
                    )
                ) 
            = (
                isHeaded,
                newSiblingTree {
                    PT.rootLabel 
                        = newCatParent <$ rootLabel,
                    PT.subForest 
                        = if isPRO treeLeftMost
                            then PT.subForest newSiblingTree
                            else newSubTree:(PT.subForest newSiblingTree)
                    }
                )
            where
                isPRO :: KTMarked -> Bool
                isPRO
                    = PT.isFilterNearTerminal $
                        (== "*PRO*") . head . KC.catlist . DMed.category
                newCatCandidates :: (ABCCat, ABCCat) 
                    -- (new leftmost head, new sibling-parent cat)
                newCatCandidates
                    | depLeftMost == DMing.Complement
                        -- (leftmost|c, head) ~~> (lm|c, lm\head)
                        = (
                            convertCatLeftMost, 
                            convertCatLeftMost ABCC.<\> givenCatParent
                            )
                        
                    | depLeftMost == DMing.Head -- trivial
                        = (givenCatParent, givenCatParent)
                    | otherwise --depLeftMost == DMing.Adjunct
                        -- (leftmost|a, head) ~~> (head/head|a, head)
                        = (
                            givenCatParent ABCC.</> givenCatParent,
                            givenCatParent
                        )
                    where 
                        convertCatLeftMost = createABCCBaseFromKC catLeftMost
                newCatLeftMostCandidate :: ABCCat
                newCatLeftMostCandidate = fst newCatCandidates
                newCatSiblingHeadCandidate :: ABCCat
                newCatSiblingHeadCandidate = snd newCatCandidates
                newSiblingTreeWithIsHeaded :: (Bool, ABCTMarked)
                newSiblingTreeWithIsHeaded -- RECURSION
                    | depLeftMost == DMing.Head -- headed!
                        = (
                            True,
                            relabelLoopRight
                                newCatSiblingHeadCandidate
                                oldTree { PT.subForest = remainder }
                        )
                    | otherwise
                        = relabelLoopLeft 
                            newCatSiblingHeadCandidate
                            oldTree { PT.subForest = remainder }
                isHeaded :: Bool
                isHeaded = fst newSiblingTreeWithIsHeaded
                newCatLeftMostFinal :: ABCCat
                newCatLeftMostFinal
                    = if isHeaded
                        then newCatLeftMostCandidate
                        else createABCCBaseFromKC catLeftMost
                newCatSiblingHeadFinal :: ABCCat
                newCatSiblingHeadFinal
                    = if isHeaded
                        then newCatSiblingHeadCandidate
                        else givenCatParent
                newSiblingTree :: ABCTMarked
                newSiblingTree = snd newSiblingTreeWithIsHeaded
                newSubTree :: ABCTMarked
                newSubTree 
                    = snd $ relabelLoopLeft newCatLeftMostFinal treeLeftMost
                newCatParent :: ABCCat
                newCatParent
                    = if isPRO treeLeftMost
                        then newCatSiblingHeadFinal
                        else givenCatParent
        relabelLoopLeft givenCatParent terminal@(PT.Node _ [])
            = (False, (givenCatParent <$) <$> terminal)
        relabelLoopRight :: ABCCat -> KTMarked -> ABCTMarked
        relabelLoopRight
            givenCatParent -- 左から降ってくる新しい親範疇
            oldTree@( -- もとのtree :: KTMarked
                PT.Node rootLabel (
                    treeLeftMost@(
                        PT.Node { 
                            PT.rootLabel = catLeftMost DMed.:|depLeftMost 
                            }
                        ):remainder -- leftmost childを取ってくる
                    )
                ) 
            = newSiblingTree {
                    PT.rootLabel 
                        = givenCatParent <$ rootLabel,
                    PT.subForest 
                        = newSubTree:(PT.subForest newSiblingTree)
                    }
            where
                newCatCandidates :: (ABCCat, ABCCat) 
                    -- (new sibling-parent cat, new leftmost head)
                newCatCandidates
                    | depLeftMost == DMing.Complement
                        -- (head, leftmost|c) ~~> (head/lm|h, lm|c)
                        = (
                            givenCatParent ABCC.</> convertCatLeftMost,
                            convertCatLeftMost
                        )
                    | otherwise --depLeftMost == DMing.Adjunct
                        -- (head, leftmost|a) ~~> (head, head\head|a)
                        = (
                            givenCatParent,
                            givenCatParent ABCC.<\> givenCatParent
                        )
                    where 
                        convertCatLeftMost = createABCCBaseFromKC catLeftMost
                newCatLeftMostFinal :: ABCCat
                newCatLeftMostFinal = snd newCatCandidates
                newCatSiblingHeadFinal :: ABCCat
                newCatSiblingHeadFinal = fst newCatCandidates
                newSiblingTree :: ABCTMarked
                newSiblingTree -- Left RECURSION
                    = relabelLoopRight
                        newCatSiblingHeadFinal
                        oldTree { PT.subForest = remainder }
                newSubTree :: ABCTMarked
                newSubTree 
                    = snd $ relabelLoopLeft newCatLeftMostFinal treeLeftMost
        relabelLoopRight givenCatParent terminal@(PT.Node _ [])
            = (givenCatParent <$) <$> terminal
        
-- # Routine
parseDoc :: String -> IO [KTMarked]
parseDoc str
    = case runParserDoc str of
        Left err 
            -> putStrLn ("\n" ++ show err) >> return []
        Right res 
            -> return res

main :: IO ()
main 
    = getContents
        >>= parseDoc
        >>= mapM_ (putStr . PTP.psdPrintDefault . relabel)