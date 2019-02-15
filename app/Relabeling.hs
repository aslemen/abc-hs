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
createABCCBaseFromKC = ABCC.createBase . KC.showCat

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
                PT.Node {
                    PT.label = catParent DMed.:| depParent,
                    PT.children 
                        = (treeLeftMost@(
                            PT.Node {
                                PT.label = catLeftMost DMed.:| depLeftMost
                                }
                            )
                        ):remainder -- leftmost childを取ってくる
                    }
                )
            = (
                isHeaded,
                newSiblingTree {
                    PT.label 
                        = newCatParent DMed.:| depParent,
                    PT.children 
                        = if isPRO treeLeftMost
                            then PT.children newSiblingTree
                            else newSubTree:(PT.children newSiblingTree)
                    }
                )
            where
                isPRO :: KTMarked -> Bool
                isPRO t =
                    length (PT.children t) == 1
                    && (KC.catlist $ DMed.category $ PT.label $ head $ PT.children t) == ["*PRO*"]
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
                                oldTree { PT.children = remainder }
                        )
                    | otherwise
                        = relabelLoopLeft 
                            newCatSiblingHeadCandidate
                            oldTree { PT.children = remainder }
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
        relabelLoopLeft givenCatParent (PT.Node label [])
            = (
                False,
                PT.Node {
                    PT.label 
                        = givenCatParent DMed.:| (DMed.dependency label),
                    PT.children = []
                    }
                )
        relabelLoopRight :: ABCCat -> KTMarked -> ABCTMarked
        relabelLoopRight
            givenCatParent -- 左から降ってくる新しい親範疇
            oldTree@( -- もとのtree :: KTMarked
                PT.Node {
                    PT.label = catParent DMed.:| depParent,
                    PT.children 
                        = (treeLeftMost@(
                            PT.Node {
                                PT.label = catLeftMost DMed.:| depLeftMost
                                }
                            )
                        ):remainder -- leftmost childを取ってくる
                    }
                )
            = newSiblingTree {
                    PT.label 
                        = givenCatParent DMed.:| depParent,
                    PT.children 
                        = newSubTree:(PT.children newSiblingTree)
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
                        oldTree { PT.children = remainder }
                newSubTree :: ABCTMarked
                newSubTree 
                    = snd $ relabelLoopLeft newCatLeftMostFinal treeLeftMost
        relabelLoopRight givenCatParent (PT.Node label [])
            = PT.Node {
                PT.label
                    = givenCatParent DMed.:| (DMed.dependency label),
                PT.children = []
            }
        
-- # Routine
parseDoc :: String -> IO [KTMarked]
parseDoc str
    = case runParserDoc str of
        Left err 
            -> putStrLn ("\n" ++ show err) >> return []
        Right res 
            -> return res

batchRelabel :: [KTMarked] -> [ABCTMarked]
batchRelabel = fmap relabel

main :: IO ()
main 
    = (
        batchRelabel 
        <$> (getContents >>= parseDoc)
    ) >>= foldr ((>>) . (putStr . show)) (return ())