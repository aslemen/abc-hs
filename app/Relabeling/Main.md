# ABCTreebank：範疇変換のプログラム
## 概要 / Introduction
範疇を変換する。
すなわち、与えられた、正規化されたKeyakiの木から、範疇を変換してABCTreebankの木をつくる。
アルゴリズムは＠＠＠のものに他ならないが、＠＠＠

## ヘッダー / Header
文字列として`Data.Text`を使用。

```haskell
{-# LANGUAGE OverloadedStrings #-}
```

```haskell
module Relabeling where

import qualified Data.Text as DT
import qualified Data.Text.IO as DTIO
import qualified Data.Text.Lazy as DTL
import qualified Data.Text.Lazy.Builder as DTLB
import qualified Data.Void as DV
import qualified Data.Set as DS

import qualified Text.Megaparsec as TMega

import qualified Control.Monad.State as CMS

import qualified DepMarking as DMing
import qualified DepMarked as DMed
import qualified DepMarked.Parser as DMedP
import qualified ABCCategory as ABCC

import qualified ParsedTree as PT
import qualified ParsedTree.Parser as PTP

import qualified PTDumpable as PTD
```

## 略記
タイプの略記は以下の通り：
```haskell
-- # Type Aliases
type ABCCat = ABCC.ABCCategory

type PlainMarked = DMed.DepMarked DT.Text
type ABCCatMarked = DMed.DepMarked ABCCat

type PlainTMarked = PT.Tree PlainMarked
type ABCTMarked = PT.Tree ABCCatMarked
```

## パーザー
```haskell
runParserPlainTMarked :: 
    String
        -> DT.Text 
        -> Either (TMega.ParseErrorBundle DT.Text DV.Void) PlainTMarked
runParserPlainTMarked
    = PTP.createFromString PTP.getDefaultTermParsers

runParserDoc :: 
    String
        -> DT.Text 
        -> Either (TMega.ParseErrorBundle DT.Text DV.Void) [PlainTMarked]
runParserDoc 
    = PTP.createDoc PTP.getDefaultTermParsers
```

## 判定関数
```haskell
checkMainPlainMarked :: DT.Text -> PlainMarked -> Bool
checkMainPlainMarked str
    = (== str) . DMed.category

checkKTIsLexicalAndHasMainKCat :: DT.Text -> PlainTMarked -> Bool
checkKTIsLexicalAndHasMainKCat
    = PT.isFilterNearTerminal . checkMainPlainMarked

isKTPRO :: PlainTMarked -> Bool
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

_dummyKCatHead :: PlainMarked 
_dummyKCatHead = "" DMed.:| DMing.Head

dropAnt :: (DS.Set ABCCat) -> ABCCat -> ABCCat
dropAnt
    forbidList
    cat@(ABCC.LeftFunctor ant conseq)
        | ant `notElem` forbidList  = dropAnt forbidList conseq
        | otherwise                 = cat
dropAnt _ cat = cat

relabel :: PlainTMarked -> ABCTMarked
relabel oldTree@(PT.Node oldTreeRootLabel _)
    = (relabelBeforeHead oldTree) `CMS.evalState` initialState
    where 
        initialState :: RelabelState
        initialState 
            = makeRelabelState
                (ABCC.BaseCategory $ DMed.category oldTreeRootLabel)
                True -- isPROToBeDropped

relabelInternal :: PlainTMarked -> WithRelabelState ABCTMarked
relabelInternal oldTree@(PT.Node _ oldTreeSubForest)
    = case oldTreeSubForest of
        _:(_:_)  -- 2 or more children
            -> relabelBeforeHead oldTree
        _        -- 0 or 1 child
            -> relabelTrivial oldTree

relabelTrivial :: PlainTMarked -> WithRelabelState ABCTMarked
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

relabelBeforeHead :: PlainTMarked -> WithRelabelState ABCTMarked
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
        relabelCompVSST :: PlainTMarked -> WithRelabelState ABCTMarked
        relabelCompVSST oldTree = do
            stParent <- CMS.get       -- state of the parent tree
            let {
                stFirstChild 
                    = makeRelabelState
                        (
                            ABCC.BaseCategory
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
        relabelAdjVSST :: PlainTMarked -> WithRelabelState ABCTMarked
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

relabelAfterHead :: PlainTMarked -> WithRelabelState ABCTMarked
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
        DMing.Adjunct
            -> relabelVSSTAdj oldTree
        DMing.AdjunctControl
            -> relabelVSSTAdj oldTree
        _   
            -> relabelVSSTAdj oldTree
        
    where
        oldTreeSubForestRemainder :: [PlainTMarked]
        oldTreeSubForestRemainder = init oldTreeSubForest
        oldTreeLastChild :: PlainTMarked
        oldTreeLastChild = last oldTreeSubForest
        relabelRealHead :: PlainTMarked -> WithRelabelState ABCTMarked
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
        relabelVSSTComp :: PlainTMarked -> WithRelabelState ABCTMarked
        relabelVSSTComp oldTree = do
            stParent <- CMS.get       -- state of the parent tree
            let {
                stLastChild
                    = makeRelabelState
                        (
                            ABCC.BaseCategory 
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
        relabelVSSTAdj :: PlainTMarked -> WithRelabelState ABCTMarked
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
                            ABCC.makeLeftAdjunct (
                                dropAnt
                                    (
                                        if (
                                            DMed.dependency 
                                            $ PT.rootLabel oldTreeLastChild
                                            ) == DMing.AdjunctControl 
                                            then DS.fromAscList [
                                                    ABCC.BaseCategory "PP-SBJ"
                                                    ,
                                                    ABCC.BaseCategory "PP-SBJ2"
                                                    ]
                                            else DS.empty
                                    )
                                    newVSSTCat 
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


isSBJControl :: PlainTMarked -> Bool -- Working now
isSBJControl tree
    = any 
        (\str -> checkKTIsLexicalAndHasMainKCat str tree)
        [
              "てあげる"
            , "てくれる"
        ]

isNonControl :: PlainTMarked -> Bool
isNonControl tree　-- ブラックリストにすべきか、ホワイトリストにすべきか？
    = any
        (\str -> checkKTIsLexicalAndHasMainKCat str tree)
        $ []
        
{-
    ======
    Routine
    ======
-}
parseDoc :: DT.Text -> IO [PlainTMarked]
parseDoc text
    = case runParserDoc "<STDIN>" text of
        Left errors
            -> DTIO.putStrLn (
                DT.pack
                    $ TMega.errorBundlePretty errors
            )
            >> return []
        Right res 
            -> return res

main :: IO ()
main 
    = DTIO.getContents
        >>= parseDoc
        >>= mapM_ 
            (
                DTIO.putStrLn 
                . DTL.toStrict
                . DTLB.toLazyText
                . PTD.psdDumpDefault
                . relabel
            )
```