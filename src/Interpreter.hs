module Interpreter where

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Syntax

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
toBool :: Expr -> Bool
toBool (Int 0) = False
toBool (Int _) = True
toBool (Float 0.0) = False
toBool (Float _) = True
toBool (StringLit "") = False
toBool (StringLit _) = True
toBool (CharLit _) = True
toBool (Boolean b) = b

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
build (BinOp And x y) = Flag $ toBool x && toBool y
build (BinOp Or x y) = Flag $ toBool x || toBool y

build (UnaryOp Not x) = Flag $ not . toBool $ x
build (Call f []) = Str ""