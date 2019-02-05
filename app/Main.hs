module Main where

import qualified ABCCategory as ABCC
import qualified ParsedTree as PT
import System.IO

type ABCT = PT.Tree ABCC.ABCCategory

parseTree :: String -> Either PT.ParseError ABCT
parseTree = PT.createFromString ABCC.parser

readPrompt :: IO String
readPrompt
    = putStr "Tree Parse>>"
        >> hFlush stdout
        >> getLine

relabel :: ABCT -> ABCT
relabel (PT.Node node children)
    | (length children) == 2 
        = PT.Node res_fc (map relabel children)
    | otherwise = PT.Node node (map relabel children)
    where
        child1 :: ABCT
        child2 :: ABCT
        child1 : (child2 : _) = children
        res_fc :: ABCC.ABCCategory
        res_fc = ABCC.reduceWithComment (PT.node child1) (PT.node child2)

parse :: String -> IO()
parse str
    = case parseTree str of
        Left err 
            -> putStrLn ("\n" ++ show err)
        Right res 
            -> print $ relabel res

interactive :: IO()
interactive
    = readPrompt
        >>= \str ->
            parse str
            >> interactive

main :: IO ()
main = interactive


