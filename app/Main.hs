module Main where

import Interpreter
import Syntax (Expr)
import Parser

import System.IO (readFile)
import System.Environment (getArgs)
import Text.Parsec (ParseError)

main :: IO ()
main = do
    file <- getArgs
    code <- readFile $ head file
    buildExprs $ map parseCode $ lines code

buildExprs :: [Either ParseError [Expr]] -> IO ()
buildExprs [] = print ""
buildExprs (c:cs) = do
                case c of 
                    Left err -> print err
                    Right expr -> do
                        print $ map build expr
                        buildExprs cs

        

-- split up blocks with a hack -y func
-- won't handle incorrect indentation
-- or nested indentation properly
-- blockSplit :: String -> [String]
-- blockSplit x = let l1:l2:ls = lines x
--                in 
--                 case take 4 l1 of
--                     "    " -> case take 4 l2 of
--                         "    " -> [l1 ++ l2] ++ blockSplit l2:ls
--                         _ -> [l1] ++ [l2] ++ blockSplit l2:ls
--                     _ -> case take 4 l2 of
--                         "    " -> [l1] ++ [l2] ++ blockSplit l2:ls
--                         _ -> [l1 ++ l2] ++ blockSplit l2:ls
