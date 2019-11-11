module Interpreter where

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Syntax

import Data.Char as C
import Numeric (showIntAtBase)


type SimpleAST = [Expr]

{-
funcs to convert custom value datatypes under `Expr`
to haskell literals. type annotations needed for numbers
to specify Num subclass
-}

unwrapInt :: Value -> Int
unwrapInt (Integ x) = x
unwrapFlt :: Value -> Float
unwrapFlt (Flt x) = x
unwrapCh :: Value -> Char
unwrapCh (Ch x) = x
unwrapStr :: Value -> String
unwrapStr (Str x) = x
unwrapStr (Ch x) = [x]
unwrapBool :: Value -> Bool
unwrapBool (Flag x) = x

class Builder a where
    convert :: Expr -> a

instance Builder Int where
    convert (Int x) = fromInteger x

instance Builder Float where
    convert (Float x) = realToFrac x

instance Builder Char where
    convert (CharLit x) = x

-- so i could make a newtype to wrap around String but
-- then i would need to change all the Expr defns also
instance Builder String where
    convert (StringLit x) = x

instance Builder Bool where
    convert (Boolean x) = x

-- make primitives truthy/falsy
-- chars are inherently true in python,
-- since they're all 1-char strings anyway,
-- so you cannot have an empty char
exprBool :: Expr -> Bool
exprBool (Int 0) = False
exprBool (Int _) = True
exprBool (Float 0.0) = False
exprBool (Float _) = True
exprBool (StringLit "") = False
exprBool (StringLit _) = True
exprBool (CharLit _) = True
exprBool (Boolean b) = b

valBool :: Value -> Bool
valBool (Integ 0) = False
valBool (Integ _) = True
valBool (Flt 0.0) = False
valBool (Flt _) = True
valBool (Str "") = False
valBool (Str _) = True
valBool (Ch _) = True
valBool (Flag x) = x
valBool (None) = False

-- lots of pattern matching ... wonder if i can make this less verbose
build :: Expr -> Value

build (CharLit x) = Ch $ convert (CharLit x)
build (Int x) = Integ $ convert (Int x)
build (Float x) = Flt $ convert (Float x)
build (StringLit x) = Str $ convert (StringLit x)
build (Boolean x) = Flag $ convert (Boolean x)

build (BinOp Add (Int x) (Int y)) = Integ $
                        convert (Int x) + convert (Int y)
build (BinOp Add (Float x) (Float y)) = Flt $
                        convert (Float x) + convert (Float y)
build (BinOp Add (Int x) (Float y)) = Flt $
                        convert (Float $ realToFrac x) 
                        + convert (Float $ realToFrac y)
build (BinOp Add (Float x) (Int y)) = Flt $
                        convert (Float $ realToFrac x)
                        + convert (Float $ realToFrac y)
build (BinOp Add (StringLit x) (StringLit y)) = Str $
                        convert (StringLit x) 
                        ++ convert (StringLit y)
build (BinOp Add (CharLit x) (CharLit y)) = Str $
                        [convert (CharLit x)]
                        ++ [convert (CharLit y)]
build (BinOp Subtract (Int x) (Int y)) = Integ $
                        convert (Int x) - convert (Int y)
build (BinOp Subtract (Float x) (Float y)) = Flt $
                        convert (Float x) - convert (Float y)
build (BinOp Subtract (Int x) (Float y)) = Flt $
                        convert (Float $ realToFrac x)
                        - convert (Float $ realToFrac y)
build (BinOp Subtract (Float x) (Int y)) = Flt $
                        convert (Float $ realToFrac x)
                        - convert (Float $ realToFrac y)
build (BinOp Mult (Int x) (Int y)) = Integ $
                        convert (Int x) * convert (Int y)
build (BinOp Mult (Float x) (Float y)) = Flt $
                        convert (Float x) * convert (Float y)
build (BinOp Mult (Int x) (Float y)) = Flt $
                        convert (Float $ realToFrac x)
                        * convert (Float $ realToFrac y)
build (BinOp Mult (Float x) (Int y)) = Flt $
                        convert (Float $ realToFrac x)
                        * convert (Float $ realToFrac y)
build (BinOp Mult (Int x) (StringLit y)) = 
                        let a = convert (Int x)
                            b = convert (StringLit y)
                        in Str $ concat $ replicate a b
build (BinOp Mult (StringLit x) (Int y)) = 
                        let a = convert (StringLit x)
                            b = convert (Int y)
                        in Str $ concat $ replicate b a
build (BinOp Mult (Int x) (CharLit y)) = 
                        let a = convert (Int x)
                            b = convert (CharLit y)
                        in Str $ replicate a b
build (BinOp Mult (CharLit x) (Int y)) = 
                        let a = convert (CharLit x)
                            b = convert (Int y)
                        in Str $ replicate b a
build (BinOp Div (Int x) (Int y)) = Integ $ 
                        div (convert (Int x)) (convert (Int y))
build (BinOp Div (Float x) (Float y)) = Flt $
                        convert (Float x) / convert (Float y)
build (BinOp Div (Int x) (Float y)) = Flt $
                        convert (Float $ realToFrac x)
                        / convert (Float y)
build (BinOp Div (Float x) (Int y)) = Flt $
                        convert (Float x)
                        / convert (Float $ realToFrac y)
-- using the `(^)` func led to some ambig. type errors, so workaround
build (BinOp Expon (Int x) (Int y)) = 
                        let a = convert (Int x)
                            b = convert (Int y)
                        in Integ $ product $ replicate b a
build (BinOp Expon (Float x) (Int y)) = 
                        let a = convert (Float x)
                            b = convert (Int y)
                        in Flt $ product $ replicate b a
build (BinOp Mod (Int x) (Int y)) = Integ $
                            convert (Int x) `mod` convert (Int y)
build (BinOp LessThan x y) = Flag $ x < y
build (BinOp GreaterThan x y) = Flag $ x > y
build (BinOp Equal x y) = Flag $ x == y
build (BinOp Inequal x y) = Flag $ x /= y
build (BinOp LessThanEqual x y) = Flag $ x <= y
build (BinOp GreaterThanEqual x y) = Flag $ x >= y
build (BinOp And x y) = Flag $ exprBool x && exprBool y
build (BinOp Or x y) = Flag $ exprBool x || exprBool y

build (UnaryOp Not x) = Flag $ not . exprBool $ x

build (If e1 e2) = case build e1 of
                        Str x -> case Flag $ valBool (Str x) of
                            Flag True -> build e2
                            Flag False -> None
                        Integ x -> case Flag $ valBool (Integ x) of
                            Flag True -> build e2
                            Flag False -> None
                        Flt x -> case Flag $ valBool (Flt x) of
                            Flag True -> build e2
                            Flag False -> None
                        Ch x -> build e2
                        Flag True -> build e2
                        Flag False -> None

{-
since Else statements are parsed as sep
exprs right now, i have to specify a boolean
expr with the Else keyword ...
so Elses exec on False conds like
how Ifs exec on True conds
-}
build (Else e1 e2) = case build e1 of
                        Str x -> case Flag $ valBool (Str x) of
                            Flag True -> None
                            Flag False -> build e2
                        Integ x -> case Flag $ valBool (Integ x) of
                            Flag True -> None
                            Flag False -> build e2
                        Flt x -> case Flag $ valBool (Flt x) of
                            Flag True -> None
                            Flag False -> build e2
                        Ch x -> None
                        Flag True -> None
                        Flag False -> build e2



build (Call "abs" (e1:[])) = case build e1 of 
                                Integ x -> Integ $ abs x
                                Flt x -> Flt $ abs x
                                _ -> None
build (Call "round" (e1:[])) = case build e1 of
                                Integ x -> Integ x
                                Flt x -> Integ $ round x
                                _ -> None
build (Call "min" [Float x, Float y]) = 
                            if x <= y then Flt $ convert (Float x) 
                            else Flt $ convert (Float y)
build (Call "min" (e1:e2:[])) = 
                    let a = build e1
                        b = build e2
                    in if a < b then a else b
build (Call "max" (e1:e2:[])) = 
                    let a = build e1
                        b = build e2
                    in if a > b then a else b
build (Call "len" (e1:[])) = case build e1 of
                                Ch x -> Integ 1
                                Str x -> Integ $ length x
                                _ -> None
-- returns type as a String, chars are 1-len strings in python
build (Call "type" (e1:[])) = case build e1 of
                                Ch x -> Str "str"
                                Str x -> Str "str"
                                Integ x -> Str "int"
                                Flt x -> Str "float"
                                Flag x -> Str "bool"
                                _ -> None
-- `lower` and `upper` are methods in python but whatever
build (Call "lower" (e1:[])) = case build e1 of
                                Ch x -> Ch $ C.toLower x
                                Str x -> Str $ map C.toLower x
                                _ -> None
build (Call "upper" (e1:[])) = case build e1 of
                                Ch x -> Ch $ C.toUpper x
                                Str x -> Str $ map C.toUpper x
                                _ -> None
-- will only work with Strings
build (Call "join" es) = let a = map convert es
                         in Str $ concat a
build (Call "bin" (e1:[])) = case build e1 of
                            Integ x -> 
                                Str $ showIntAtBase 2 C.intToDigit x ""
                            _ -> None
build (Call "any" es) = let a = map build es
                        in Flag $ any (==True) $ map valBool a
build (Call "all" es) = let a = map build es
                        in Flag $ all (==True) $ map valBool a
build (Call "bool" (e1:[])) = Flag $ exprBool e1
-- we expect correct input (ie within range) for the next 2 funcs
build (Call "ord" (e1:[])) = Integ $ C.ord . unwrapCh . build $ e1
build (Call "chr" (e1:[])) = Ch $ C.chr . unwrapInt . build $ e1
build (Call "str" (e1:[])) = let a = build e1
                             in case a of
                                Integ x -> Str $ show x
                                Ch x -> Str $ show x
                                Str x -> Str x
                                Flt x -> Str $ show x
                                Flag x -> Str $ show x
                                _ -> None
build (Call "globals" []) = Str
                        "haskell doesn't maintain global vars"
build (Call "help" []) = Str "help message"
build (Call "hex" (e1:[])) = case build e1 of
                                Integ x -> 
                                    Str $ showIntAtBase 16 C.intToDigit x ""
build (Call "oct" (e1:[])) = case build e1 of
                                Integ x -> 
                                    Str $ showIntAtBase 8 C.intToDigit x ""
build (Call "pow" (e1:e2:[])) = case build e1 of
                                    Integ x -> case build e2 of
                                        Integ y -> Integ $ x ^ y
                                        _ -> None
                                    Flt x -> case build e2 of
                                        Integ y -> Flt $ x ^ y
                                        _ -> None
                                    _ -> None
build (Call "sum" es) =  Integ $ sum $ map convert es
build (Call "id" (e1:[])) = case build e1 of
                                Ch x -> Ch x
                                Str x -> Str x
                                Integ x -> Integ x
                                Flt x -> Flt x
                                Flag x -> Flag x
                                a -> a
build (Call "none" _) = None




build _ = None
