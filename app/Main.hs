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
    -- prettyParsingPrint $ map parseCode $ lines code
    printExprs $ map parseCode $ lines code

printExprs :: [Either ParseError [Expr]] -> IO ()
printExprs [] = return ()
printExprs (c:cs) = do
                case c of 
                    Left err -> print err
                    Right exprn -> do
                        buildExprs $ map build exprn
                        printExprs cs

buildExprs :: [Value] -> IO ()
buildExprs [] = return ()
buildExprs (e:es) = do
                case e of
                    Integ x -> print $ unwrapInt (Integ x)
                    Str x -> print $ unwrapStr (Str x)
                    Ch x -> print $ unwrapStr (Ch x)
                    Flt x -> print $ unwrapFlt (Flt x)
                    Flag x -> print $ unwrapBool (Flag x)
                    None -> return ()
                buildExprs es

