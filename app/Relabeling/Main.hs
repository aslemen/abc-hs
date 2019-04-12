{-# LANGUAGE OverloadedStrings #-}

{- 
The Pipeline:
- pretreatments
- head-marking (Haskell? tsurgeon?)
- **relabelling**
-}

module Relabeling where

import qualified Data.Text as DT
import qualified Data.Text.IO as DTIO

import qualified Text.Megaparsec as TMega

import qualified Control.Monad.State as CMS

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
    = ABCC.BaseCategory 
        . DT.pack
        . (PTP.psdPrint (PTP.Option PTP.Pretty PTP.Minimal))

checkMainKCat :: String -> KCat -> Bool
checkMainKCat str
    = (== str) . head . KC.catlist

checkMainKCatMarked :: String -> KCatMarked -> Bool
checkMainKCatMarked str
    = (checkMainKCat str) . DMed.category

checkKTIsLexicalAndHasMainKCat :: String -> KTMarked -> Bool
checkKTIsLexicalAndHasMainKCat
    = PT.isFilterNearTerminal . checkMainKCatMarked

isKTPRO :: KTMarked -> Bool
isKTPRO tree 
    = or
        $ checkKTIsLexicalAndHasMainKCat 
            <$> ["*PRO*", "*T*"] 
            <*> [tree]

data RelabelState
    = RelabelState {
        givenParentCat :: ABCCat,   -- Readonly
        isHeadFound :: Bool,        -- Writeonly
        isPROToBeDropped :: Bool    -- Readonly
    } deriving (Eq, Show)
    
makeRelabelState :: ABCCat -> Bool -> RelabelState
makeRelabelState cat pro
    = RelabelState {
        givenParentCat = cat,
        isHeadFound = True, -- arbitrary
        isPROToBeDropped = pro
    }
getIsHeadFound :: RelabelState -> Bool
getIsHeadFound = isHeadFound

type WithRelabelState a = CMS.State RelabelState a

_dummyKCatHead :: KCatMarked 
_dummyKCatHead = (KC.createBase []) DMed.:| DMing.Head

dropAnt :: [ABCCat] -> ABCCat -> ABCCat
dropAnt
    forbidList
    cat@(ABCC.LeftFunctor ant conseq)
        | ant `notElem` forbidList  = dropAnt forbidList conseq
        | otherwise                 = cat
dropAnt _ cat = cat

relabel :: KTMarked -> ABCTMarked
relabel oldTree@(PT.Node oldTreeRootLabel _)
    = (relabelBeforeHead oldTree) `CMS.evalState` initialState
    where 
        initialState :: RelabelState
        initialState 
            = makeRelabelState
                (createABCCBaseFromKC $ DMed.category oldTreeRootLabel)
                True -- isPROToBeDropped

relabelInternal :: KTMarked -> WithRelabelState ABCTMarked
relabelInternal oldTree@(PT.Node _ oldTreeSubForest)
    = case oldTreeSubForest of
        _:(_:_)  -- 2 or more children
            -> relabelBeforeHead oldTree
        _        -- 0 or 1 child
            -> relabelTrivial oldTree

relabelTrivial :: KTMarked -> WithRelabelState ABCTMarked
relabelTrivial
    oldTree@(
        PT.Node {
            PT.rootLabel = oldTreeRootLabel,
            PT.subForest = oldTreeSubForest
        }
    ) = do
        stParent <- CMS.get
        let {
            newTree 
                = PT.Node {
                    PT.rootLabel
                        = (givenParentCat stParent) <$ oldTreeRootLabel
                    ,
                    PT.subForest
                        = relabel <$> oldTreeSubForest
                            -- RECURSION from the very beginning
                }
        }
        CMS.modify $ \st -> st { isHeadFound = False }
        return newTree

relabelBeforeHead :: KTMarked -> WithRelabelState ABCTMarked
relabelBeforeHead
    oldTree@(
        PT.Node {
            PT.rootLabel = oldTreeRootLabel,
            PT.subForest 
                = oldTreeSubForest@(
                    oldTreeFirstChild
                    :oldTreeSubForestRemainder
                )
        }
    )
    = case DMed.dependency $ PT.rootLabel oldTreeFirstChild of
        DMing.Complement -> relabelCompVSST oldTree
        DMing.Head       -> relabelAfterHead oldTree
        _                -> relabelAdjVSST oldTree
    where
        relabelCompVSST :: KTMarked -> WithRelabelState ABCTMarked
        relabelCompVSST oldTree = do
            stParent <- CMS.get       -- state of the parent tree
            let {
                stFirstChild 
                    = makeRelabelState
                        (
                            createABCCBaseFromKC
                                $ DMed.category
                                    $ PT.rootLabel oldTreeFirstChild
                        )
                        True -- to drop PROs in complements
            }
            let { 
                newTreeFirstChild
                    = (relabelInternal oldTreeFirstChild) `CMS.evalState` stFirstChild 
                    -- RECURSION
                    -- local evaluation
            }
            let {
                stVSST
                    = makeRelabelState
                        (
                             (DMed.category $ PT.rootLabel newTreeFirstChild) 
                                ABCC.<\> (givenParentCat stParent)
                        )
                        $ isPROToBeDropped stParent
            }
            let {
                (newTreeVSST, stAfterVSST)
                    = relabelBeforeHead PT.Node {
                            PT.rootLabel = _dummyKCatHead
                            ,
                            PT.subForest = oldTreeSubForestRemainder
                        }
                        `CMS.runState` stVSST
                        -- CONTINUING
            }
            let isHeaded = getIsHeadFound stAfterVSST -- NOTE: reading from WRITER
            CMS.modify $ \st -> st { isHeadFound = isHeaded } -- NOTE: writing WRITER
            if isHeaded
                then if (not $ isPROToBeDropped stParent) 
                            || (not $ isKTPRO oldTreeFirstChild)
                    then return $ PT.Node {
                            PT.rootLabel
                                = (givenParentCat stParent) -- 結局stParent.givenParentCatになるので、これでよい
                                    <$ oldTreeRootLabel
                            ,
                            PT.subForest
                                = case PT.subForest newTreeVSST of
                                    _:(_:_)      -> [newTreeFirstChild, newTreeVSST]
                                    onlyChild:[] -> [newTreeFirstChild, onlyChild]
                                    []           -> [newTreeFirstChild]
                            }
                    else 
                        return 
                            $ (
                                \node -> node {
                                PT.rootLabel 
                                    = (DMed.category $ PT.rootLabel node)
                                        <$ oldTreeRootLabel
                                })
                            (
                                case PT.subForest newTreeVSST of
                                    _:(_:_)      -> newTreeVSST
                                    onlyChild:[] -> onlyChild
                                    []           -> newTreeVSST
                            )
                else 
                    relabelTrivial oldTree
        relabelAdjVSST :: KTMarked -> WithRelabelState ABCTMarked
        relabelAdjVSST oldTree = do
            stParent <- CMS.get       -- state of the parent tree
            let {
                stVSST
                    = makeRelabelState
                        (givenParentCat stParent)
                        $ isPROToBeDropped stParent
            }
            let {
                (newTreeVSST, stAfterVSST)
                    = relabelBeforeHead 
                        PT.Node {
                            PT.rootLabel = _dummyKCatHead
                            ,
                            PT.subForest = oldTreeSubForestRemainder
                        } 
                        `CMS.runState` stVSST
                -- CONTINUING
            }
            let {
                newVSSTCat
                    = DMed.category $ PT.rootLabel newTreeVSST
            }
            let {
                stFirstChild 
                    = makeRelabelState
                        (ABCC.makeRightAdjunct newVSSTCat) -- head/head (VSST)
                        False -- to NOT drop PROs in complements
            }
            let {
                newTreeFirstChild
                    = (relabelInternal oldTreeFirstChild) `CMS.evalState` stFirstChild
                        -- RECURSION
                        -- local evaluation
            }
            let {
                newTreeFirstChildCat
                    = DMed.category $ PT.rootLabel newTreeFirstChild
            }
            let isHeaded = getIsHeadFound stAfterVSST
            CMS.modify $ \st -> st { isHeadFound = isHeaded }
            if isHeaded
                then if (not $ isPROToBeDropped stParent) 
                            || (not $ isKTPRO oldTreeFirstChild)
                    then return $ PT.Node {
                            PT.rootLabel
                                = (
                                    newTreeFirstChildCat
                                        ABCC.<^> newVSSTCat
                                ) <$ oldTreeRootLabel
                            ,
                            PT.subForest
                                = case PT.subForest newTreeVSST of
                                    _:(_:_)      -> [newTreeFirstChild, newTreeVSST]
                                    onlyChild:[] -> [newTreeFirstChild, onlyChild]
                                    []           -> [newTreeFirstChild]
                            }
                    else return 
                            $ (
                                \node -> node {
                                PT.rootLabel 
                                    = (DMed.category $ PT.rootLabel node)
                                        <$ oldTreeRootLabel
                                })
                            (
                                case PT.subForest newTreeVSST of
                                    _:(_:_)      -> newTreeVSST
                                    onlyChild:[] -> onlyChild
                                    []           -> newTreeVSST
                            )
                else
                    relabelTrivial oldTree
relabelBeforeHead oldTree@(PT.Node _ []) = do
    newTree <- relabelTrivial oldTree
    CMS.modify $ \st -> st { isHeadFound = False }
    return newTree

relabelAfterHead :: KTMarked -> WithRelabelState ABCTMarked
relabelAfterHead
    oldTree@(
        PT.Node {
            PT.rootLabel = oldTreeRootLabel,
            PT.subForest = oldTreeSubForest@(_:_)
        }
    )
    = case DMed.dependency $ PT.rootLabel oldTreeLastChild of
        DMing.Complement 
            -> relabelVSSTComp oldTree
        DMing.Head 
            -> case oldTreeSubForest of
                _:(_:_) -> relabelVSSTAdj oldTree -- extra heads are treated as adjuncts
                _:[]    -> relabelRealHead oldTree
        _   -> relabelVSSTAdj oldTree
        
    where
        oldTreeSubForestRemainder :: [KTMarked]
        oldTreeSubForestRemainder = init oldTreeSubForest
        oldTreeLastChild :: KTMarked
        oldTreeLastChild = last oldTreeSubForest
        relabelRealHead :: KTMarked -> WithRelabelState ABCTMarked
        relabelRealHead oldTree = do
            stInit <- CMS.get
            newHead <- relabelInternal oldTreeLastChild -- RECURSION
            let {
                newTree 
                    = PT.Node {
                        PT.rootLabel = givenParentCat stInit <$ oldTreeRootLabel
                        ,
                        PT.subForest = [newHead]
                    }
                }
            CMS.modify $ \st -> st {isHeadFound = True}
            return newTree
        relabelVSSTComp :: KTMarked -> WithRelabelState ABCTMarked
        relabelVSSTComp oldTree = do
            stParent <- CMS.get       -- state of the parent tree
            let {
                stLastChild
                    = makeRelabelState
                        (
                            createABCCBaseFromKC 
                                $ DMed.category
                                    $ PT.rootLabel oldTreeLastChild
                        )
                        True -- to drop PROs in complements
            }
            let {
                newTreeLastChild
                    = (relabelInternal oldTreeLastChild) `CMS.evalState` stLastChild 
                        -- RECURSION
                        -- local evaluation
            }
            let {
                stVSST
                    = makeRelabelState
                        (
                            (givenParentCat stParent)
                                ABCC.</> (
                                    DMed.category $ PT.rootLabel newTreeLastChild
                                )
                        )
                        $ isPROToBeDropped stParent
            }
            let {
                newTreeVSST
                    = relabelAfterHead 
                        PT.Node { -- RECURSION!
                                PT.rootLabel = _dummyKCatHead
                                ,
                                PT.subForest = oldTreeSubForestRemainder
                            }
                        `CMS.evalState` stVSST
            }
            CMS.modify $ \st -> st { isHeadFound = True } -- NOTE: writing WRITER
            (if ((not $ isPROToBeDropped stParent) || (not $ isKTPRO oldTreeLastChild))
                    then return $ PT.Node {
                            PT.rootLabel
                                = (givenParentCat stParent) -- 結局stParent.givenParentCatになるので、これでよい
                                    <$ oldTreeRootLabel
                            ,
                            PT.subForest
                                = case PT.subForest newTreeVSST of
                                    _:(_:_)      -> [newTreeVSST, newTreeLastChild]
                                    onlyChild:[] -> [onlyChild, newTreeLastChild]
                                    []           -> [newTreeLastChild]
                            }
                    else 
                        return 
                            $ (
                                \node -> node {
                                PT.rootLabel 
                                    = (DMed.category $ PT.rootLabel node)
                                        <$ oldTreeRootLabel
                                })
                            (
                                case PT.subForest newTreeVSST of
                                    _:(_:_)      -> newTreeVSST
                                    onlyChild:[] -> onlyChild
                                    []           -> newTreeVSST
                            ))
        relabelVSSTAdj :: KTMarked -> WithRelabelState ABCTMarked
        relabelVSSTAdj oldTree = do
            stParent <- CMS.get       -- state of the parent tree
            let {
                stVSST
                    = makeRelabelState
                        (givenParentCat stParent)
                        $ isPROToBeDropped stParent
            }
            let {
                newTreeVSST
                    = relabelAfterHead
                        PT.Node {
                                PT.rootLabel = _dummyKCatHead
                                ,
                                PT.subForest = oldTreeSubForestRemainder
                            }
                        `CMS.evalState` stVSST
                        -- RECURSION!
            }
            let {
                newVSSTCat
                    = DMed.category $ PT.rootLabel newTreeVSST
            }
            let {
                stLastChild 
                    = makeRelabelState
                        (
                            ABCC.makeLeftAdjunct (case () of
                                _
                                    | isNonControl oldTreeLastChild
                                        -> newVSSTCat
                                    | isSBJControl oldTreeLastChild
                                        -> dropAnt [ABCC.BaseCategory "PPs"] newVSSTCat
                                    
                                    | otherwise
                                        -> dropAnt [] newVSSTCat
                                )
                        ) -- (VSST) head\head 
                        False -- to NOT drop PROs in complements
            }
            let {
                newTreeLastChild
                    = (relabelInternal oldTreeLastChild) `CMS.evalState` stLastChild
                        -- RECURSION
                        -- local evaluation
            }
            let {
                newTreeLastChildCat
                    = DMed.category $ PT.rootLabel newTreeLastChild
            }
            CMS.modify $ \st -> st { isHeadFound = True }
            (if ((not $ isPROToBeDropped stParent) || (not $ isKTPRO oldTreeLastChild))
                    then return $ PT.Node {
                            PT.rootLabel
                                = (
                                    newVSSTCat
                                        ABCC.<^> newTreeLastChildCat
                                ) <$ oldTreeRootLabel
                            ,
                            PT.subForest
                                = case PT.subForest newTreeVSST of
                                    _:(_:_)      -> [newTreeVSST, newTreeLastChild]
                                    onlyChild:[] -> [onlyChild, newTreeLastChild]
                                    []           -> [newTreeLastChild]
                            }
                    else return 
                            $ (
                                \node -> node {
                                PT.rootLabel 
                                    = (DMed.category $ PT.rootLabel node)
                                        <$ oldTreeRootLabel
                                })
                            (
                                case PT.subForest newTreeVSST of
                                    _:(_:_)      -> newTreeVSST
                                    onlyChild:[] -> onlyChild
                                    []           -> newTreeVSST
                            ))
relabelAfterHead oldTree@(PT.Node _ []) = do
    newTree <- relabelTrivial oldTree
    CMS.modify $ \st -> st { isHeadFound = True }
    return newTree


isSBJControl :: KTMarked -> Bool -- Working now
isSBJControl tree
    = any 
        (\str -> checkKTIsLexicalAndHasMainKCat str tree)
        [
              "てあげる"
            , "てくれる"
        ]

isNonControl :: KTMarked -> Bool
isNonControl tree　-- ブラックリストにすべきか、ホワイトリストにすべきか？
    = any
        (\str -> checkKTIsLexicalAndHasMainKCat str tree)
        $ []
        
{-
    ======
    Routine
    ======
-}
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
        >>= mapM_ (putStrLn . PTP.psdPrintDefault . relabel)