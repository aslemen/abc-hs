module Linting where

import System.IO
import Data.Monoid

import qualified ABCCategory as ABCC
import qualified ParsedTree as PT
import qualified ParsedTree.Parser as PTP
import qualified Options.Applicative as OPT

import qualified Data.Text as DT
import qualified Data.Text.Lazy as DTL
import qualified Data.Text.Lazy.Builder as DTLB
import qualified Data.Text.IO as DTIO

import qualified Text.Megaparsec as TMega

import qualified PTDumpable as PTD

-- # Type Aliases
type PlainTree = PT.Tree DT.Text

-- # Tree Parser

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
    Jobs
    =====
-}
parseDoc :: DT.Text -> IO [PlainTree]
parseDoc text
    = case PTP.createDoc PTP.getDefaultTermParsers "<STDIN>" text of
        Left errors
            -> DTIO.putStrLn 
                (
                    DT.pack 
                        $ TMega.errorBundlePretty errors
                )
                >> return []
        Right res 
            -> return res
{-
    ======
    Routines
    =====
-}
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
            )