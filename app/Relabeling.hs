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

checkMainKCat :: String -> KCat -> Bool
checkMainKCat str
    = (== str) . head . KC.catlist

checkMainKCatMarked :: String -> (DMed.DepMarked KCat) -> Bool
checkMainKCatMarked str
    = (checkMainKCat str) . DMed.category

checkKTIsLexicalAndHasMainKCat :: String -> (PT.Tree (DMed.DepMarked KCat)) -> Bool
checkKTIsLexicalAndHasMainKCat
    = PT.isFilterNearTerminal . checkMainKCatMarked

isKTPRO :: (PT.Tree (DMed.DepMarked KCat)) -> Bool
isKTPRO tree 
    = foldr (||) False
        $ checkKTIsLexicalAndHasMainKCat <$> ["*PRO*", "*T*"] <*> [tree]

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
                    treeLeftmost@(
                        PT.Node { 
                            PT.rootLabel = catLeftmost DMed.:|depLeftmost 
                            }
                        ):remainder -- leftmost childを取ってくる
                    )
                ) 
            = result 
            where
                {-
Terminology:
    A Tree contains: 
        - KeyakiCat (old)
        - tentative ABCCat
        - final ABCCat
        - children

    (Virtual) Parent Tree (VPT)
        |--
        |--
        |-- ============== Below Focused
        |-- The leftmost subtree
        |--   ----------
        |--   | ......    <- The remainder
        |--   | ......       (which is considered to be contained in a virtual subtree: the virtual sibling-subtree (VSST))
        |--   | ......
        |--   ----------
            ==============================
                -} 
                {- 
Calculation Step 1:
    - calculate the tentative ABCCat of the letfmost subtree
        - case KeyakiCat leftmost subtree of
            _ :| complement     -> keep the name of the KeyakiCat, converted to a basic ABCCat
            _ :| head, adjunct  -> discard the KeyakiCat 
                -}
                newCatLeftmostTentative1 :: ABCCat 
                newCatLeftmostTentative1
                    = case depLeftmost of 
                        DMing.Complement
                        -- leftmost|c (head) ~~> leftmost|c
                            -> createABCCBaseFromKC catLeftmost
                        DMing.Head -- trivial
                            -> givenCatParent
                        _  -- depLeftmost == DMing.Adjunct
                            -- leftmost|a (head) ~~> head/head|a
                            -> givenCatParent ABCC.</> givenCatParent
                {-
Calculation Step 2:
    - calculate the final leftmost subtree
        - adopted in any case
        - calculate its final ABCCat, taking into consideration PROs inside the subtree 
                -}
                defaultTreeLeftmost :: ABCTMarked
                defaultTreeLeftmost
                    = snd 
                        $ relabelLoopLeft 
                            (createABCCBaseFromKC catLeftmost)
                            treeLeftmost
                newTreeLeftmost :: ABCTMarked
                newTreeLeftmost 
                    = snd 
                        $ relabelLoopLeft 
                            newCatLeftmostTentative1
                            treeLeftmost
                newCatLeftmostTentative2 :: ABCCat
                newCatLeftmostTentative2
                    = DMed.category $ PT.rootLabel newTreeLeftmost
                {-
Calculation Step 3:
    - calculate the final virtual subling ABCCat
    - calculate the final virtual sibling subtree 
        - adopted only when the head is found
                -}
                newCatVSSTTentative :: ABCCat
                newCatVSSTTentative        
                    | depLeftmost == DMing.Complement
                        -- (leftmost|c) head ~~> leftmost\head
                        = newCatLeftmostTentative2 ABCC.<\> givenCatParent
                    | depLeftmost == DMing.Head
                        = givenCatParent
                    | otherwise --depLeftmost == DMing.Adjunct
                        -- (leftmost|a) head) ~~> head
                        = givenCatParent
                newVSSTWithIsHeaded :: (Bool, ABCTMarked)
                newVSSTWithIsHeaded -- RECURSION
                    | depLeftmost == DMing.Head -- headed!
                        = (
                            True,
                            relabelLoopRight
                                newCatVSSTTentative
                                oldTree { PT.subForest = remainder }
                        )
                    | otherwise
                        = relabelLoopLeft 
                            newCatVSSTTentative 
                            oldTree { PT.subForest = remainder }
                newVSST :: ABCTMarked
                newVSST = snd newVSSTWithIsHeaded
                newCatVSST :: ABCCat
                newCatVSST = DMed.category $ PT.rootLabel newVSST
                isHeaded :: Bool
                isHeaded = fst newVSSTWithIsHeaded
                {-
Calculation Step 4:
    - Collecting the results
                -}
                newVPT :: ABCTMarked
                newVPT
                    | isKTPRO treeLeftmost
                        = newVSST
                    | otherwise
                        = newVSST {
                            PT.rootLabel
                                = 
                                -- if isHeaded
                                    -- then givenCatParent <$ rootLabel
                                    -- else createABCCBaseFromKC <$> rootLabel
                                    givenCatParent <$ rootLabel
                            ,
                            PT.subForest
                                = (
                                    if isHeaded
                                        then newTreeLeftmost
                                        else defaultTreeLeftmost
                                ):(PT.subForest newVSST)
                        }
                result :: (Bool, ABCTMarked)
                result = (isHeaded, newVPT)
        relabelLoopLeft _ terminal@(PT.Node _ [])
            = (False, (createABCCBaseFromKC <$>) <$> terminal)
        relabelLoopRight :: ABCCat -> KTMarked -> ABCTMarked
        relabelLoopRight
            givenCatParent -- 左から降ってくる新しい親範疇
            oldTree@( -- もとのtree :: KTMarked
                PT.Node rootLabel (
                    treeLeftmost@(
                        PT.Node { 
                            PT.rootLabel = catLeftmost DMed.:|depLeftmost 
                            }
                        ):remainder -- leftmost childを取ってくる
                    )
                ) 
            = result
            where
                dropAnt :: [ABCCat] -> ABCCat -> ABCCat
                dropAnt
                    forbidList
                    cat@(ABCC.LeftFunctor ant conseq _)
                        | ant `notElem` forbidList  = dropAnt forbidList conseq
                        | otherwise                 = cat
                dropAnt _ cat = cat
                multiplyLeft :: ABCCat -> ABCCat
                multiplyLeft cat = cat ABCC.<\> cat
                matchLexTreeLeftmost :: String -> Bool
                matchLexTreeLeftmost str 
                    = checkKTIsLexicalAndHasMainKCat str treeLeftmost
                newCatLeftmost :: ABCCat
                newCatLeftmost
                    = case depLeftmost of
                        DMing.Complement -- (head, leftmost|c) ~~> (head/lm|h, lm|c)
                            -> createABCCBaseFromKC catLeftmost
                        DMing.Adjunct
                            | matchLexTreeLeftmost "てあげる"
                                -> multiplyLeft $ dropAnt [ABCC.createBase "PPs"] givenCatParent
                            | otherwise
                                -> multiplyLeft $ dropAnt [] givenCatParent
                        _ -- (head) leftmost|?? ~~> head\head|?? by default
                            -> multiplyLeft givenCatParent
                newTreeLeftmost :: ABCTMarked
                newTreeLeftmost 
                    = snd $ relabelLoopLeft newCatLeftmost treeLeftmost
                newCatVSST :: ABCCat
                newCatVSST 
                    = case depLeftmost of
                        DMing.Complement -- (head, leftmost|c) ~~> (head/lm|h, lm|c)
                            -> givenCatParent ABCC.</> newCatLeftmost
                        _  -- (head) leftmost|?? ~~> head\head|?? by default
                            -> givenCatParent
                newVSST :: ABCTMarked
                newVSST -- Left RECURSION
                    = relabelLoopRight
                        newCatVSST
                        oldTree { PT.subForest = remainder }

                result :: ABCTMarked
                result
                    = newVSST {
                        PT.rootLabel 
                            = givenCatParent <$ rootLabel,
                        PT.subForest 
                            = newTreeLeftmost:(PT.subForest newVSST)
                        }
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