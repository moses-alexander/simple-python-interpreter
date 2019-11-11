module Parser where

{-
each parser returns a monad (a structure
with a stream, state, identity monad, and 
return type. the identity monad can be subbed
for somn else (like an exception monad to
handle more specific interpretation errors)
the parser monad is needed to be passed
to `parse` so that the stream, state,
underlying monad, and type can be tracked
across the entire stream.
-}

import Lexer
import Syntax

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as E
import qualified Text.Parsec.Token as T

{-
parse op, return the func that matches w the op
with the proper association.
`op' n` parses the operator,
returns `f` in Parser monadic context
and constructs an instance of Parsec.Expr.Operator
using the `E.*fix` constructor.
-}

binary n f a = E.Infix (do { op' n; return (BinOp f) }) a
prefix n f = E.Prefix (do { op' n; return (UnaryOp f) })
-- python has no postfix ops i think.

opTable = [ [ prefix "not" Not ]
          , [ binary "and" And E.AssocLeft
            , binary "or" Or E.AssocLeft ]
          , [ binary "==" Equal E.AssocLeft
            , binary "!=" Inequal E.AssocLeft
            , binary "<" LessThan E.AssocLeft
            , binary ">" GreaterThan E.AssocLeft
            , binary "<=" LessThanEqual E.AssocLeft
            , binary ">=" GreaterThanEqual E.AssocLeft ]
          , [ binary "**" Expon E.AssocLeft ]
          , [ binary "*" Mult E.AssocLeft
            , binary "/" Div E.AssocLeft
            , binary "%"  Mod E.AssocLeft ]
          , [ binary "+" Add E.AssocLeft
            , binary "-" Subtract E.AssocLeft ]
          ]

-- returns Parser containing all the lexing funcs for tokenising
-- and parsing rules defined below
-- all of the rules are in the Parsec monad,
-- they call the lexing funcs which return 
-- monads with the corresponding types applied.
expr :: Parser Expr
expr = E.buildExpressionParser opTable tries

comma = do
    reserved' ","
    

int =  do
    i <- int'
    return $ Int i

float =  do
    f <- float'
    return $ Float f

character = do
    c <- char'
    return $ CharLit c

string'' = do
    s <- string'
    return $ StringLit s

true = do
    reserved' "True"
    return $ Boolean True

false = do
    reserved' "False"
    return $ Boolean False

var = do
    v <- ident
    return $ Var v

assign = do
    name <- ident
    op' "="
    val <- expr
    return $ Assign name val

call = do
    name <- ident
    args <- parens' $ commaSep' (  try var 
                               <|> try string''
                               <|> try true
                               <|> try false
                               <|> try character
                               <|> try float
                               <|> try int ) -- gets args from btwn parens
    return $ Call name args

func = do
    reserved' "def"
    name <- ident
    args <- parens' $ commaSep' var
    colon'
    body <- expr -- gets statements below def
    return $ Func name args body

for = do
    reserved' "for"
    counter <- var
    reserved' "in"
    reserved' "range"
    range <- parens' var
    body <- expr
    return $ For range body

with = do
    reserved' "with"
    val <- var
    reserved' "as"
    alias <- ident
    body <- expr
    return $ With alias val body

if' = do
    reserved' "if"
    cond <- expr
    colon'
    ifTrue <- expr
    return $ If cond ifTrue

else' = do
    reserved' "else"
    cond <- expr
    colon'
    ifFalse <- expr
    return $ Else cond ifFalse

{-
`try` looks ahead and checks if a parser succeeds or fails.
if it succeeds, retains that value. else, 
moves to next parser. these parsers then become
the terms for the expression builder above.
this is the parser combinator 
"that accepts several parsers as input
and returns a new parser as its output". (from wikipedia)
uses `mplus` since Parser is a monad and monoid.
-}
tries :: Parser Expr
tries = try assign
    <|> try call
    <|> try var
    <|> try func
    <|> try int
    <|> try true
    <|> try false
    <|> try character
    <|> try string''
    <|> try float
    <|> try for
    <|> try with
    <|> try if'
    <|> try else'
    <|> parens' expr

cont :: Parser a -> Parser a
cont p = do -- prevents infinite look ahead
    T.whiteSpace lexer -- ignores leading whitespace
    r <- p -- takes input parser and binds it to r
    eof -- returns an ignored Parser and kills r @ eof
    return r

parseExpr :: Parser Expr
parseExpr = expr
    <|> try func

parseLine :: Parser [Expr]
parseLine = many $ do
    stmnt <- parseExpr
    -- handles semicolons for mulltistatement lines or just ignores it
    op' ";" <|> op' ""
    return stmnt

-- `cont expr` returns a `Parser` monad
-- `parse` runs that monad over stdin
-- returns multiple parsed expressions
parseCode :: String -> Either ParseError [Expr]
parseCode s = parse (cont parseLine) "<stdin>" s

prettyParsingPrint :: [Either ParseError [Expr]] -> IO ()
prettyParsingPrint [] = print ""
prettyParsingPrint (x:xs) = case x of
                              Left e -> print $ show e
                              Right [] -> print ""
                              Right _ -> do
                                        print x
                                        prettyParsingPrint xs