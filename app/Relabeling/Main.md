# ABCTreebank：範疇変換のプログラム
## 概要 / Introduction
与えられた，正規化されたKeyakiの木から，範疇を変換して，ABC Treebankの木をつくる．
根本にあるアルゴリズムは＠＠＠のものに他ならないが，
    コントロールされた項の扱いなどのために，より複雑になっている．

## ヘッダー / Header
文字列としては`Data.Text`を使用．同時に，多相的な文字列リテラルを許す．
```haskell
{-# LANGUAGE OverloadedStrings #-}
```

モジュール名を設定する．
```haskell
module Relabeling where
```

基本的なモジュールのインポートをする．
```haskell
import qualified System.IO as S

import qualified Data.Text as DT
import qualified Data.Text.IO as DTIO
import qualified Data.Void as DV
import qualified Data.Set as DS
```

パーザーコンビネーターとして，Megaparsecを使用する．
```haskell
import qualified Text.Megaparsec as TMega

import qualified Control.Monad.State as CMS
```

ABC Treebankのためのモジュールを読み込む．
```haskell
import qualified DepMarking as DMing
import qualified DepMarked as DMed
import qualified DepMarked.Parser as DMedP
import qualified ABCCategory as ABCC

import qualified ParsedTree as PT
import qualified ParsedTree.Parser as PTP
```

結果の出力として，prettyprinterを用いる．
```haskell
import qualified Data.Text.Prettyprint.Doc as PDoc
import qualified Data.Text.Prettyprint.Doc.Render.Text as PDocRT
```

## データ型の別名 / Aliases of Data Types
木や範疇に関するデータ型の略名をつける．

`ABCCat`：ABC Treebankの（変換後の，組み合わせ論理の）範疇を表現する．
```haskell
type ABCCat = ABCC.ABCCategory
```

`...Marked`：範疇で，dependency markingが付加されたものを表現する．
このうち`Plain`とは，変換前の（Keyakiの）範疇（実際，ただの`DT.Text`）を表現する．
```haskell
type PlainMarked = DMed.DepMarked DT.Text
type ABCCatMarked = DMed.DepMarked ABCCat
```

`...TMarked`：木で，ノードがdependency marked categoryであるようなものを
表現する．
```haskell
type PlainTMarked = PT.Tree PlainMarked
type ABCTMarked = PT.Tree ABCCatMarked
```

## パーザー実行者 / Parser Executors
組み合わせパーザーを実行する関数．

`runParserDoc name source`：
入力`source`から，
**dependency markを受けたKeyakiの（0個以上の）統語木**を読み込み，
パージングをし，プログラム内部の表現に変換する．
入力源の名前は`name`である（実際のパージングには影響はない）．
結果として，パージングが成功の場合，
`PlainTMarked`（Keyaki木）のリストが`Right`に包まれて得られる．
失敗の場合，MegaParsecのエラー型が`Left`に包まれて得られる．
```haskell
runParserDoc :: 
    String
        -> DT.Text 
        -> Either (TMega.ParseErrorBundle DT.Text DV.Void) [PlainTMarked]
runParserDoc 
    = PTP.createDoc PTP.getDefaultTermParsers
```

## 判定関数 / Decisive Functions
Keyakiの範疇や語彙に関して判定を行う関数たちである．

`checkMainPlainMarked`：
    Keyaki範疇（dependency markingを含む）が
    与えられた範疇（dependency markingは問わない）と同じかどうか？
```haskell
checkMainPlainMarked :: DT.Text -> PlainMarked -> Bool
checkMainPlainMarked str
    = (== str) . DMed.category
```

`checkKTIsLexicalAndHasMainKCat`：
    あるKeyaki部分木（dependency markingを含む）が，
    終端ノードと語彙ノードだけを持ち，
    かつ語彙ノードとして与えられた語彙を持つかどうか？
```haskell
checkKTIsLexicalAndHasMainKCat :: DT.Text -> PlainTMarked -> Bool
checkKTIsLexicalAndHasMainKCat
    = PT.isFilterNearTerminal . checkMainPlainMarked
```

`isKTPRO`：あるKeyaki部分木（dependency markingを含む）が，
    終端ノードと語彙ノードだけを持ち，
    かつ語彙ノードとして空範疇（`*PRO*`または`*T*`）を持つかどうか？
```haskell
isKTPRO :: PlainTMarked -> Bool
isKTPRO tree 
    = or
        $ checkKTIsLexicalAndHasMainKCat 
            <$> ["*PRO*", "*T*"] 
            <*> [tree]
```

## 変換アルゴリズムの概要
変換とは，まだ変換を受けていない部分木を持つあるKeyaki（部分）木
    （dependency markingを受けている）を対象とするものである．
変換においては，ルートノードの候補はあらかじめ（再帰的に，外から）ABC Treebank範疇
    として与えられている．
ルートノードは，通常，与えられたものがそのまま最終的なものとなるが，
    部分木として空範疇が与えらているような場合，
    最終的なルートノードはそれに応じて変更されることがある．
```
Parent (as ABCCat)
    |- Subtree 1
    |- Subtree 2
    |- ...
    |- Subtree n
```
従って，変換とは，以下を引数とする関数であるべきである：
- 最終ルートノードの候補：`ABCCat`
- 子木のリスト：`[PlainTMarked]`
    - ただし，現状では，（捨てられる運命にある）ルートノードも一緒に
        引数として渡されることになる．
        これについて以後変更することがありうる．
- その他のパラメータ

また，変換はtop-downで，引数に加え，さらに以下の要素に左右される：
    部分木の変換の結果
    （すなわち，全体の結果は，部分木の内部（特に，空範疇）にも依存する）．
変換の返り値は，`ABCTMarked`であるべきである．

## 変換状態を表すデータ型 / Data Types Representing Intermediate States
`RelabelState`は変換プロセスの中途の状態を表現する．

- `givenParentCat`：引数の1つ，期待されるルートノードの範疇の候補
- `isHeadFound`：いまの部分木の中で，head `''h`とマークされる範疇をルートに持つ部分木が**ちょうど1つ**存在するか
- `isPROToBeDropped`：空範疇が削除されるべきか否か

```haskell
data RelabelState
    = RelabelState {
        givenParentCat :: ABCCat,   -- Readonly
        isHeadFound :: Bool,        -- Writeonly
        isPROToBeDropped :: Bool    -- Readonly
    } deriving (Eq, Show)
```

`RelabelState`のSmart Constructorとして以下がある：
`makeRelabelState rootCat pro`は，
`isHeadFound`が`True`なのをデフォルトとして
`RelabelState`を生成する．
```haskell
makeRelabelState :: ABCCat -> Bool -> RelabelState
makeRelabelState cat pro
    = RelabelState {
        givenParentCat = cat,
        isHeadFound = True, -- arbitrary
        isPROToBeDropped = pro
    }
```

`RelabelState`の射影のエイリアスとして以下がある：
```haskell
getIsHeadFound :: RelabelState -> Bool
getIsHeadFound = isHeadFound
```

`RelabelState`は，部分木に付加される状態（State）だと思うことができる．
これを実際に，1つのモナドだとする．
```haskell
type WithRelabelState a = CMS.State RelabelState a
```

## ＠＠＠
```haskell
_dummyKCatHead :: PlainMarked 
_dummyKCatHead = "" DMed.:| DMing.Head

dropAnt :: (DS.Set ABCCat) -> ABCCat -> ABCCat
dropAnt
    forbidList
    cat@(ABCC.LeftFunctor ant conseq)
        | ant `notElem` forbidList  = dropAnt forbidList conseq
        | otherwise                 = cat
dropAnt _ cat = cat
```

## 変換 / Relabeling
ルート木の変換は，`relabel`関数でなされる．
`relabel`は，dependency markingのなされている木を取り，
（dependency markingを残した）ABC Treebankの木を返す．

`relabel`関数の主な機能は2つである：
- 初期状態をつくる
- 最初の子木たちに関して，（後述する）主要部未決定relabelを行う．
```haskell
relabel :: PlainTMarked -> ABCTMarked
relabel oldTree@(PT.Node oldTreeRootLabel _)
    = (relabelBeforeHead oldTree) `CMS.evalState` initialState
    where 
        initialState :: RelabelState
        initialState 
            = makeRelabelState
                (ABCC.BaseCategory $ DMed.category oldTreeRootLabel)
                True -- isPROToBeDropped
```

`relabelInternal`は，relabelの再帰において呼び出される．
機能はだた1つで，ルーティングを行う：
- もし，与えられた木の子木が2つ以上の子を持つなら，
    主要部未決定relabelを行う
- そうでないなら，（後述する）不relabelを行う．
```haskell
relabelInternal :: PlainTMarked -> WithRelabelState ABCTMarked
relabelInternal oldTree@(PT.Node _ oldTreeSubForest)
    = case oldTreeSubForest of
        _:(_:_)  -- 2 or more children
            -> relabelBeforeHead oldTree
        _        -- 0 or 1 child
            -> relabelTrivial oldTree
```

以下，範疇変換アルゴリズムにおける，3つの異なる変換関数とそれらの間の
    遷移について決める．

### 不relabel / Non-Relabeling
主要部がない，もしくは，2つ以上の子がないような木については，
    relabelを**しない**ことを行う．
```haskell
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
```

### 主要部未決定relabel
範疇変換アルゴリズムは，子木を左から右に向かって走査し，
    主要部を見つけようとする．
主要部が見つかった場合，`relabelAfterHead`関数に遷移する．
終いまで見つからなかった場合，状態`RelabelState`のうち
    `isHeadFound`を`False`として，再帰呼び出しから呼び出し元にこのことを
    「伝達」する．`False`「シグナル」を受けて，
    主要部変換を「中止」する．

なお，主要部だとされる子木が複数あるときは，最左のものを主要部とみなし，
    他を補部だとみなす．
このような状況はそもそもあってはならない．


```haskell
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
```

子木が1つ以上ある場合，その子`oldTreeFirstChild`のノードの
    dependency markingについて場合分けをする．
- complement `''c`である場合：`relabelCompVSST`
- head `''h`である場合：`relabelAfterHead`
- その他：`relabelAdjVSST`
```haskell
    = case DMed.dependency $ PT.rootLabel oldTreeFirstChild of
        DMing.Complement -> relabelCompVSST oldTree
        DMing.Head       -> relabelAfterHead oldTree
        _                -> relabelAdjVSST oldTree
    where
```

`relabelCompVSST`について述べる．
```haskell
        relabelCompVSST :: PlainTMarked -> WithRelabelState ABCTMarked
        relabelCompVSST oldTree = do
```
この関数に遷移してきたとき，注目されている最左の子木は補部`''c`である．
```
Parent (as ABCCat)
    |- Subtree i ''c 
    |- Subtree i+1  ---|
    |- ...             |-- ''h
    |- Subtree n    ---|
```
それより右側の木は，それらをひっくるめて，
    新たな中間木（最左子木に対しては主要部となる；_VSST_ と呼ぶ）
    をつくることになる．
二分木化もついでに行われることにもなる．
```
Parent (as ABCCat)
    |- Subtree i ''c 
    |- VSST ''h
        |- Subtree i+1
        |- ... 
        |- Subtree n
```
（ただし，主要部がこれ以降見つからなかった場合は，このような変形は中止される．）

範疇の決定は直ちにはできない．なぜなら，子木とVSSTのそれぞれに，
    空範疇が含まれることがあり，
    それらの削除によって範疇がまた変わることがあるからである．
従って，子木とVSSTのそれぞれについて，
    与えられた親の範疇を用いて**仮の**範疇を算出し，
    その範疇をもとに，`relabel`を再帰的に行うことをする．
    その結果として，新たな，真の子木とVSSTが得られる．


まず，状態`stParent`を入手する．
```haskell
            stParent <- CMS.get       -- state of the parent tree
```
次に，子状態`stFirstChild`を作り，子木の変換を済ませる．
```haskell
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
```
その上で，VSST状態`stVSST`を作り，VSST木の変換（`relabelBeforeHead`の継続）
    を済ませる．
継続した結果，主要部子木の有無を知る必要があるので，状態を取り戻す（`stAfterVSST`）
    ことをする．
```haskell
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
```
主要部子木の有無は，呼び出し元に伝えなければならないので，それを把握し，
    受け渡された状態`stParent`を変更する．
```haskell
            let isHeaded = getIsHeadFound stAfterVSST -- NOTE: reading from WRITER
            CMS.modify $ \st -> st { isHeadFound = isHeaded } -- NOTE: writing WRITER
```

主要部子木があることが分かったとき，実際に変換を行う．
```haskell
            if isHeaded
```
ここで，空範疇削除が要求され，かつ主要部子木自身が空範疇である，か否かでさらに場合分けをする．
もしそうでないなら，
```haskell
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
```
もしそうであるなら，主要部子木がまるごと削除され，VSSTそのものが返り値となる．
```
[DEL] Parent (as ABCCat)
    |- [DEL] Subtree i ''c -- *PRO* 
    |- [RETURN] VSST: <(Subtree i)\Parent>''h
        |- Subtree i+1
        |- ... 
        |- Subtree n
```

```haskell
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
```
主要部子木がないことが分かったとき，今までの計算結果を破棄し，
    不relabelに遷移する．
```haskell
                else 
                    relabelTrivial oldTree
```

`relabelAdjVSST`について述べる．
```haskell
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
```

子木が空リストである場合，それは，主要部未決定relabelのまま子木上の走査が終了した，ということである．
すなわち，主要部がなかった，ということである．
このような場合，`isHeadFound`を`False`について，呼び出し元に状態を付き返す．
```haskell
relabelBeforeHead oldTree@(PT.Node _ []) = do
    newTree <- relabelTrivial oldTree
    CMS.modify $ \st -> st { isHeadFound = False }
    return newTree
```

### 主要部決定後relabel
```haskell
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
```

```haskell
relabelAfterHead oldTree@(PT.Node _ []) = do
    newTree <- relabelTrivial oldTree
    CMS.modify $ \st -> st { isHeadFound = True }
    return newTree
```

# 実行ルーティン / Routines in Execution
```haskell
parseDoc :: DT.Text -> IO [PlainTMarked]
parseDoc text
    = case runParserDoc "<STDIN>" text of
        Left errors
            -> DTIO.hPutStrLn S.stderr (
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
        >>= (PDocRT.putDoc . PDoc.vsep . (map (PDoc.pretty . relabel)))
```