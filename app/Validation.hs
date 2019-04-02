module Validation where

import System.IO
import Data.Monoid

import qualified ABCCategory as ABCC
import qualified ParsedTree as PT
import qualified PTPrintable as PTP

import qualified Options.Applicative as OPT

-- # Type Aliases
type ABCCom = ABCC.ABCCategoryCommented
type ABCComT = PT.Tree ABCCom

-- # Tree Parser
runParserTree :: String -> Either PT.ParseError ABCComT
runParserTree = PT.createFromString ABCC.parser

runParserDoc :: String -> Either PT.ParseError [ABCComT]
runParserDoc = PT.createDoc ABCC.parser

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

-- # Actual Job
relabel :: ABCComT -> ABCComT
relabel (PT.Node node children)
    | (length children) == 2 
        = PT.Node {
            PT.rootLabel = res,
            PT.subForest = map relabel children
        }
    | otherwise 
        = PT.Node {
            PT.rootLabel = node,
            PT.subForest = map relabel children
        }
    where
        child1 :: ABCComT
        child2 :: ABCComT
        child1 : (child2 : _) = children
        res :: ABCCom
        res 
            = join 
                $ ABCC.reduceWithLog
                    <$> (PT.rootLabel child1)
                    <*> (PT.rootLabel child2)
            where join m = m >>= id

batchRelabel :: [ABCComT] -> [ABCComT]
batchRelabel
    = fmap relabel

parseDoc :: String -> IO [ABCComT]
parseDoc str
    = case runParserDoc str of
        Left err 
            -> putStrLn ("\n" ++ show err) >> return []
        Right res 
            -> return res

-- # Main Procedure
main :: IO ()
main 
    = getContents
        >>= parseDoc
        >>= mapM_ (putStr . PTP.psdPrintDefault . relabel)