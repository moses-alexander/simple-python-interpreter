module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as T

{- 
`Parser` has an empty state, only reads String.
this is a super simple parser so not impl. other
stream types. a state type is applied to the `Parser`
in the "type" parsing funcs below the lexer.
the lexer tokenises the "input string" of code,
probably going to be context-free. will hack on some 
parsing funcs for multi-line python statements
(like `if...else`, `for`, etc).
-}

lexer :: T.TokenParser ()
lexer = T.makeTokenParser style
    where
        ops = ["\n", "+", "*", "-", "!=", "and", "=",
               "or", "not", "==", "<", ">", "**", ";",
               ">=", "<=" ]
        names = [ "if", "else", "for", "with", "in", "def", "range",
                  "True", "False", "None", "," ]
        style = emptyDef { T.commentLine = "#"
                         , T.reservedOpNames = ops
                         , T.reservedNames = names
                         , T.caseSensitive = True
                         }

{-
uses `makeTokenParser` to create a `TokenParser` record using the
style defined in `lexer`. the below funcs are added to the `lexer`
record by calling the Parsec funcs (integer, reserved, etc)
and applying `lexer` as a param. these are of type `Parser _`
where _ is the type of Token lexeme parser.
-}

int' = T.integer lexer
float' = T.float lexer
char' = T.charLiteral lexer
string' = T.stringLiteral lexer
ident = T.identifier lexer
colon' = T.colon lexer
op' :: String -> Parser ()
op' = T.reservedOp lexer
parens' :: Parser a -> Parser a
parens' = T.parens lexer
commaSep' :: Parser a -> Parser [a]
commaSep' = T.commaSep lexer
reserved' :: String -> Parser ()
reserved' = T.reserved lexer

-- func to resolve "True"/"False" strings to Bools
boolean' :: String ->  Maybe Bool
boolean' "True" = Just True
boolean' "False" = Just False
boolean' _ = Nothing
