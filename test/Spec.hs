import Interpreter
import Syntax
import Control.Monad.State

main :: IO ()
main = do
    print "testing: ..."
    print "test primitive resolution"
    print $ build (Int 4)
    print $ build (CharLit 'a')
    print $ build (StringLit "abc")
    print $ build $ Float 3
    print $ build $ Boolean True
    print "test Addition bin op"
    print $ build $ BinOp Add (Int 5) (Int 4)
    print $ build $ BinOp Add (Float 5) (Float 4)
    print $ build $ BinOp Add (Int 5) (Float 4)
    print $ build $ BinOp Add (Float 5) (Int 4)
    print $ build $ BinOp Add (StringLit "abc") (StringLit "def")
    print $ build $ BinOp Add (CharLit 'a') (CharLit 'b')
    print "test Subtraction bin op"
    print $ build $ BinOp Subtract (Int 5) (Int 4)
    print $ build $ BinOp Subtract (Float 5) (Float 4)
    print $ build $ BinOp Subtract (Int 5) (Float 4)
    print $ build $ BinOp Subtract (Int 5) (Float 4)
    print "test Multiplication bin op"
    print $ build $ BinOp Mult (Int 5) (Int 4)
    print $ build $ BinOp Mult (Float 5) (Float 4)
    print $ build $ BinOp Mult (Int 3) (StringLit "abc")
    print $ build $ BinOp Mult (StringLit "abc") (Int 3)
    print $ build $ BinOp Mult (Int 3) (CharLit 'g')
    print $ build $ BinOp Mult (CharLit 'g') (Int 3)
    print $ build $ BinOp Mult (Int 5) (Float 4)
    print $ build $ BinOp Mult (Float 5) (Int 4)
    print "test Exponentiation bin op"
    print $ build $ BinOp Expon (Int 5) (Int 4)
    print $ build $ BinOp Expon (Float 5) (Int 4)
    print "test modulo bin op"
    print $ build $ BinOp Mod (Int 5) (Int 4)
    print "test LT bin op"
    print $ build $ BinOp LessThan (Int 5) (Int 4)
    print $ build $ BinOp LessThan (Float 5) (Float 4)
    print $ build $ BinOp LessThan (StringLit "abc") (StringLit "def")
    print $ build $ BinOp LessThan (CharLit 'a') (CharLit 'b')
    print "test GT bin op"
    print $ build $ BinOp GreaterThan (Int 5) (Int 4)
    print $ build $ BinOp GreaterThan (Float 5) (Float 4)
    print $ build $ BinOp GreaterThan (StringLit "abc") (StringLit "def")
    print $ build $ BinOp GreaterThan (CharLit 'a') (CharLit 'b')
    print "test EQ bin op"
    print $ build $ BinOp Equal (Int 5) (Int 4)
    print $ build $ BinOp Equal (Float 5) (Float 4)
    print $ build $ BinOp Equal (StringLit "abc") (StringLit "abc")
    print $ build $ BinOp Equal (CharLit 'a') (CharLit 'b')
    print "test NEQ bin op"
    print $ build $ BinOp Inequal (Int 5) (Int 4)
    print $ build $ BinOp Inequal (Float 5) (Float 4)
    print $ build $ BinOp Inequal (StringLit "abc") (StringLit "abc")
    print $ build $ BinOp Inequal (CharLit 'a') (CharLit 'b')
    print "test LTE bin op"
    print $ build $ BinOp LessThanEqual (Int 5) (Int 4)
    print $ build $ BinOp LessThanEqual (Float 5) (Float 4)
    print $ build $ BinOp LessThanEqual (StringLit "abc") (StringLit "abc")
    print $ build $ BinOp LessThanEqual (CharLit 'a') (CharLit 'b')
    print "test GTE bin op"
    print $ build $ BinOp GreaterThanEqual (Int 5) (Int 4)
    print $ build $ BinOp GreaterThanEqual (Float 5) (Float 4)
    print $ build $ BinOp GreaterThanEqual (StringLit "abc") (StringLit "abc")
    print $ build $ BinOp GreaterThanEqual (CharLit 'a') (CharLit 'b')
    print "test AND bin op"
    print $ build $ BinOp And (Int 0) (Int 3)
    print $ build $ BinOp And (Float 3) (Float 4)
    print $ build $ BinOp And (StringLit "") (StringLit "abc")
    print $ build $ BinOp And (CharLit 'a') (CharLit 'b')
    print $ build $ BinOp And (Boolean True) (Boolean True)
    print $ build $ BinOp And (Boolean True) (Boolean False)
    print $ build $ BinOp And (Boolean False) (Boolean False)
    print "test OR bin op"
    print $ build $ BinOp Or (Int 0) (Int 3)
    print $ build $ BinOp Or (Float 3) (Float 4)
    print $ build $ BinOp Or (StringLit "") (StringLit "")
    print $ build $ BinOp Or (CharLit 'a') (CharLit 'b')
    print $ build $ BinOp Or (Boolean True) (Boolean True)
    print $ build $ BinOp Or (Boolean True) (Boolean False)
    print $ build $ BinOp Or (Boolean False) (Boolean False)
    print "test NOT unary op"
    print $ build $ UnaryOp Not (Boolean False)
    print $ build $ UnaryOp Not (StringLit "abc")
    print $ build $ UnaryOp Not (Int 0)
    print "test Division bin op"
    print $ build $ BinOp Div (Float 3) (Float 4)
    print $ build $ BinOp Div (Float 3) (Int 4)
    print $ build $ BinOp Div (Int 3) (Float 4)
    print $ build $ BinOp Div (Int 3) (Int 4)
    print "test IF stmnts"
    print $ build $ If (BinOp GreaterThan (Int 6) (Int 3)) (Call "abs" [Int (-4)])
    print $ build $ If (BinOp LessThan (Int 6) (Int 3)) (Call "abs" [Int (5)])
    print $ build $ If (StringLit "") (Call "abs" [Int (-6)])
    print "test ELSE stmnts"
    print $ build $ Else (BinOp GreaterThan (Int 6) (Int 3)) (Call "abs" [Int (-4)])
    print $ build $ Else (BinOp LessThan (Int 6) (Int 3)) (Call "abs" [Int (5)])
    print $ build $ Else (StringLit "") (Call "abs" [Int (-6)])


    print "test called funcs"
    print $ build $ Call "abs" [Int (-4)]
    print $ build $ Call "abs" [Float (-4)]
    print $ build $ Call "round" [Int 4]
    print $ build $ Call "round" [Float 4.2]
    print $ build $ Call "min" [Float 4.2, Float 4.1]
    print $ build $ Call "min" [Int 3, Int 5]
    print $ build $ Call "min" [StringLit "abc", StringLit "def"]
    print $ build $ Call "min" [CharLit 'r', CharLit 'd']
    print $ build $ Call "min" [StringLit "abc", StringLit "abc"]
    print $ build $ Call "max" [Float 4.2, Float 4.1]
    print $ build $ Call "max" [Int 3, Int 5]
    print $ build $ Call "max" [StringLit "abc", StringLit "def"]
    print $ build $ Call "max" [CharLit 'r', CharLit 'd']
    print $ build $ Call "max" [StringLit "abc", StringLit "abc"]
    print $ build $ Call "len" [StringLit "abc"]
    print $ build $ Call "len" [CharLit 'a']
    print $ build $ Call "type" [CharLit 'a']
    print $ build $ Call "type" [Int (-4)]
    print $ build $ Call "type" [Float (-4)]
    print $ build $ Call "type" [StringLit "abc"]
    print $ build $ Call "lower" [CharLit 'A']
    print $ build $ Call "lower" [StringLit "AbC"]
    print $ build $ Call "upper" [CharLit 'a']
    print $ build $ Call "upper" [StringLit "aBc"]
    print $ build $ Call "join" [StringLit "abc", StringLit "abc"]
    print $ build $ Call "bin" [Int 45]
    print $ build $ Call "any" [Int 1, Int 2, Int 0]
    print $ build $ Call "any" [StringLit "" , StringLit ""]
    print $ build $ Call "all" [Int 1, Int 2, Int 0]
    print $ build $ Call "all" [Float 1.1, Float 3.4]
    print $ build $ Call "bool" [Int 0]
    print $ build $ Call "bool" [Int 1]
    print $ build $ Call "ord" [CharLit 'a']
    print $ build $ Call "chr" [Int 97]
    print $ build $ Call "str" [Int 97]
    print $ build $ Call "str" [Boolean True]
    print $ build $ Call "hex" [Int 45]
    print $ build $ Call "oct" [Int 45]
    print $ build $ Call "pow" [Int 1, Int 2]
    print $ build $ Call "sum" [Int 1, Int 2]
    print $ build $ Call "id" [Int 45]
    print $ build $ Call "id" [CharLit 'a']
    print $ build $ Call "none" []
    print $ build $ Call "none" [Int 6, Int 8]