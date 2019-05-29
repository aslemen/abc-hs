module PTDiff where

import qualified ParsedTree as PT
import qualified ParsedTree.Parser as PTP
import qualified Options.Applicative as OPT

import qualified Data.Text as DT
import qualified Data.Text.Lazy as DTL
import qualified Data.Text.Lazy.Builder as DTLB
import qualified Data.Text.IO as DTIO

import qualified Data.TreeDiff.Class as DTC
import qualified Data.TreeDiff.Pretty as DTP

import qualified Text.Megaparsec as TMega

-- # Type Aliases
type PlainTree = PT.Tree DT.Text

-- # Commandline Option Parser
data Options 
    = Options {
        file1 :: FilePath,
        file2 :: FilePath
    } deriving (Show, Eq)

opFile1 :: OPT.Parser FilePath
opFile1 
    = OPT.strArgument $ (
        OPT.metavar "FILE1"
        <> OPT.action "file"
    )

opFile2 :: OPT.Parser FilePath
opFile2 
    = OPT.strArgument $ (
        OPT.metavar "FILE2"
        <> OPT.action "file"
    )

opOptions :: OPT.Parser Options
opOptions
    = Options <$> opFile1 <*> opFile2

opOptionsWithInfo :: OPT.ParserInfo Options
opOptionsWithInfo
    = OPT.info opOptions $ (
        OPT.fullDesc
        <> OPT.progDesc "Diff for Parsed Trees"
    )

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
    = OPT.execParser opOptionsWithInfo
        >>= \opt -> 
            (DTIO.readFile $ file1 opt)
            >>= parseDoc
            >>= \doc1 ->
                (DTIO.readFile $ file2 opt)
                >>= parseDoc
                >>= \doc2 ->
                    (
                        DTIO.putStrLn 
                            $ DT.pack 
                            $ show
                            $ DTP.ansiWlBgEditExpr 
                            $ DTC.ediff doc1 doc2
                    )