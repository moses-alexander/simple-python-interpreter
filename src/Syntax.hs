module Syntax
  (  Expr(..)
  , Value(..)
  , Op(..)
  )

where

data Expr
    = Var String
    | Int Integer
    | Float Double
    | StringLit String
    | CharLit Char
    | Boolean Bool
    | BinOp Op Expr Expr
    | UnaryOp Op Expr
    | Call String [Expr]
    | Func String [Expr] Expr
    | If Expr Expr
    | Else Expr
    | For Expr Expr
    | With String Expr Expr
    | Assign String Expr
    deriving (Show, Eq, Ord)

data Value
    = Integ Int
    | Flt Float
    | Str String
    | Ch Char
    | Flag Bool
    deriving (Show, Ord, Eq)

data Op
    = Add
    | Subtract
    | Mult
    | Div
    | Expon
    | Mod
    | And
    | Or
    | Not
    | Equal
    | LessThan
    | GreaterThan
    | Inequal
    | LessThanEqual
    | GreaterThanEqual
    deriving (Show, Eq, Ord)



