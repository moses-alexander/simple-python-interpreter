module Main where

import Interpreter
import Syntax
import Parser

import System.IO (readFile)
import System.Environment (getArgs)
import Text.Parsec (ParseError)

main :: IO ()
main = do
    file <- getArgs
    code <- readFile $ head file
    printExprs $ map parseCode $ lines code

printExprs :: [Either ParseError [Expr]] -> IO ()
printExprs [] = return ()
printExprs (c:cs) = do
                case c of 
                    Left err -> print err
                    Right expr -> do
                        buildExprs $ map build expr
                        printExprs cs

buildExprs :: [Value] -> IO ()
buildExprs [] = return ()
buildExprs (e:es) = do
                case e of
                    Integ x -> print $ unwrapInt (Integ x)
                    Str x -> print $ unwrapStr (Str x)
                    Ch x -> print $ unwrapStr (Ch x)
                    Flt x -> print $ unwrapStr (Flt x)
                    Flag x -> print $ unwrapBool (Flag x)
                    _ -> print "error"
                buildExprs es
        

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
