{-# LANGUAGE OverloadedStrings #-}

module Validation where

import qualified Data.Text as DT
import qualified Data.Text.IO as DTIO
import qualified Data.Text.Lazy as DTL
import qualified Data.Text.Lazy.Builder as DTLB
import qualified Data.Void as DV
import qualified Data.Set as DS

import qualified Text.Megaparsec as TMega

import qualified Control.Monad.State as CMS

import qualified ABCCategory.Parser as ABCP
import qualified ABCCategory as ABCCat
import qualified ABCComment as ABCCom
import qualified ParsedTree as PT
import qualified ParsedTree.Parser as PTP

import qualified Data.Text.Prettyprint.Doc as PDoc
import qualified Data.Text.Prettyprint.Doc.Render.Text as PDocRT

import qualified Options.Applicative as OPT

-- # Type Aliases
type ABCCat = ABCCat.ABCCategory
type ABCT = PT.Tree ABCCat

type ABCCom = ABCCom.ABCComment ABCCat.ABCCategory
type ABCComT = PT.Tree ABCCom

{-
    ======
    Tree Parsers
    ======
-}
runParserTree :: String 
    -> DT.Text
    -> Either (TMega.ParseErrorBundle DT.Text DV.Void) ABCT
runParserDoc :: String 
    -> DT.Text
    -> Either (TMega.ParseErrorBundle DT.Text DV.Void) [ABCT]
runParserTree = PTP.createFromString PTP.getDefaultTermParsers
runParserDoc = PTP.createDoc PTP.getDefaultTermParsers

-- # Commandline Option Parser
-- よくわかっていないので、放っておくことにする。しばらくはstdin/stdoutを使う。
-- 並列処理とかできればもっと嬉しい。
data Options 
    = Options {
    inputPath :: FilePath,
    outputPath :: FilePath
    } deriving (Show)

parserInputPath :: OPT.Parser FilePath
parserInputPath
    = OPT.strArgument $ mconcat [
        OPT.help "The path to the input file",
        OPT.metavar "input_path",
        OPT.action "file"
    ]

parserOutputPath :: OPT.Parser FilePath
parserOutputPath
    = OPT.strArgument $ mconcat [
        OPT.help "The path of output",
        OPT.metavar "output_path",
        OPT.action "file"
    ]
    

parserOptions :: OPT.Parser Options
parserOptions
    = (<*>) OPT.helper (
        Options <$> parserInputPath <*> parserOutputPath
    )
        

parserOptionsWithInfo :: OPT.ParserInfo Options
parserOptionsWithInfo 
    = OPT.info parserOptions $ mconcat [
        OPT.fullDesc,
        OPT.progDesc "ABC Tree Checker"
        ]

{-
    ======
    Actual Job
    ======
-}
relabel :: ABCT -> ABCComT
relabel (PT.Node node children@(child1:child2:[]))
    = PT.Node {
        PT.rootLabel 
            = ABCCat.reduceWithLog
                (PT.rootLabel child1)
                (PT.rootLabel child2)
        ,
        PT.subForest = map relabel children
        }
relabel (PT.Node node children@(_:_:_:_))
    = PT.Node {
        PT.rootLabel 
            = ABCCom.ABCComment node "FAIL:NONBin"
        ,
        PT.subForest = map relabel children
    }
relabel (PT.Node node children) -- 1 or 0 child
    = PT.Node {
        PT.rootLabel
            = ABCCom.ABCComment node ""
        ,
        PT.subForest = map relabel children
    }

parseDoc :: DT.Text -> IO [ABCT]
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

-- # Main Procedure
main :: IO ()
main 
    = DTIO.getContents
        >>= parseDoc
        >>= mapM_ (
            PDocRT.putDoc
            . PDoc.pretty
            . relabel 
            )