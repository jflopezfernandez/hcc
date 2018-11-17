
module Lexer(Operator(..), DataType(..), Token(..), tokenize, lookAtNextToken, acceptToken) where

import Data.Char

data Operator = Plus
              | Minus
              | Times
              | Div
              | Modulo
              | Negation
              | Exponentiation
              | LessThan
              | GreaterThan
              | SizeOf
        deriving (Show, Eq)

operator :: [Char] -> Operator
operator [c] | c == '+' = Plus
             | c == '-' = Minus
             | c == '*' = Times
             | c == '/' = Div
             | c == '%' = Modulo
             | c == '!' = Negation
             | c == '^' = Exponentiation
             | c == '<' = LessThan
             | c == '>' = GreaterThan

operator str | str == "sizeof" = SizeOf
             | otherwise = error $ "Unknown operator input: " ++ str

operatorToString :: Operator -> String
operatorToString Plus           = "+"
operatorToString Minus          = "-"
operatorToString Times          = "*"
operatorToString Div            = "/"
operatorToString Modulo         = "%"
operatorToString Negation       = "!"
operatorToString Exponentiation = "^"
operatorToString LessThan       = "<"
operatorToString GreaterThan    = ">"
operatorToString SizeOf         = "sizeof"

data DataType = TypeInt
              | TypeChar
              | TypeFloat
              | TypeDouble
              | TypeVoid
              | TypeBoolean
        deriving (Show, Eq)

datatype :: String -> DataType
datatype str
    | str == "int"      = TypeInt
    | str == "char"     = TypeChar
    | str == "float"    = TypeFloat
    | str == "double"   = TypeDouble
    | str == "void"     = TypeVoid
    | str == "bool"     = TypeBoolean
    | otherwise         = error $ "Unknown data type: " ++ str

showDataType :: DataType -> String
showDataType TypeInt     = "int"
showDataType TypeChar    = "char"
showDataType TypeFloat   = "float"
showDataType TypeDouble  = "double"
showDataType TypeVoid    = "void"
showDataType TypeBoolean = "bool"

dataTypesList :: [String]
dataTypesList = ["int","char","float","double","void", "bool"]

data PreprocessorDirective = DirectiveIf
                           | DirectiveEndIf
                           | DirectiveInclude
                           | DirectiveIfNotDefined
        deriving (Show, Eq)

preprocessorDirective :: String -> PreprocessorDirective
preprocessorDirective str
    | str == "#if" = DirectiveIf
    | str == "#endif" = DirectiveEndIf
    | str == "#include" = DirectiveInclude
    | str == "#ifndef" = DirectiveIfNotDefined
    | otherwise = error $ "Unknown directive: " ++ str

showPreprocessorDirective :: PreprocessorDirective -> String
showPreprocessorDirective dir
    | dir == DirectiveIf = "#if"
    | dir == DirectiveEndIf = "#endif"
    | dir == DirectiveInclude = "#include"
    | dir == DirectiveIfNotDefined = "#ifndef"
    | otherwise = error $ "Unknown preprocessor directive."

listPreprocessorDirectives :: [String]
listPreprocessorDirectives = ["#if","#endif","#include","#ifndef"]

data Token = TokenOperator Operator
           | TokenIdentifier String
           | TokenDataType DataType
           | TokenNumber Double
           | TokenLeftParen
           | TokenRightParen
           | TokenEndOfLine
           | TokenAssignment
           | TokenPreprocessorDirective PreprocessorDirective
           | TokenEndOfInput
        deriving (Show, Eq)

showTokenContent :: Token -> String
showTokenContent (TokenOperator op) = operatorToString op
showTokenContent (TokenIdentifier str) = str
showTokenContent (TokenNumber n) = show n
showTokenContent TokenLeftParen = show "("
showTokenContent TokenRightParen = show ")"
showTokenContent TokenEndOfLine = show ';'
showTokenContent TokenAssignment = show '='
showTokenContent (TokenDataType t) = showDataType t
showTokenContent (TokenPreprocessorDirective dir) = showPreprocessorDirective dir
showTokenContent TokenEndOfInput = "[End of Input]"

lookAheadChar :: String -> Char
lookAheadChar [] = ' '
lookAheadChar [x] = x
lookAheadChar (x:_) = x

digits :: String -> (String, String)
digits str = digs "" str
    where
        digs :: String -> String -> (String, String)
        digs acc [] = (acc, [])
        digs acc (c:cs) | isDigit c =
                                let
                                    (acc', cs') = digs acc cs
                                in
                                    (c:acc', cs')
                        | otherwise = (acc, c:cs)

number :: Char -> String -> [Token]
number c cs =
    let
        (digs, cs') = digits cs
    in
        TokenNumber (read (c:digs)) : tokenize cs'

alnums :: String -> (String, String)
alnums str = als "" str
    where
        als acc [] = (acc, [])
        als acc (c:cs)  | isAlphaNum c =
                            let (acc', cs') = als acc cs
                            in (c:acc', cs')
                        | c == '_' =
                            let (acc', cs') = als acc cs
                            in (c:acc', cs')
                        | otherwise = (acc, c:cs)

identifier :: Char -> String -> [Token]
identifier c cs =
    let
        (str, cs') = alnums cs
    in
        if (lookAheadChar cs') == '(' then
            TokenIdentifier (c:str) : tokenize cs'
        else
            if elem (c:str) dataTypesList then
                TokenDataType (datatype (c:str)) : tokenize cs'
            else
                TokenIdentifier (c:str) : tokenize cs'

directive :: Char -> String -> [Token]
directive c cs =
    let
        (str, cs') = alnums cs
    in
        if elem (c:str) listPreprocessorDirectives then
            TokenPreprocessorDirective (preprocessorDirective (c:str)) : tokenize cs'
        else
            TokenIdentifier (c:str) : tokenize cs'

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:cs)
    | elem c "+-/*/%!^" = TokenOperator (operator [c]) : tokenize cs
    | c == ';' = TokenEndOfLine : tokenize cs
    | c == '(' = TokenLeftParen : tokenize cs
    | c == ')' = TokenRightParen : tokenize cs
    | c == '=' = TokenAssignment : tokenize cs
    | c == '#' = directive c cs
    | isDigit c = number c cs
    | isAlpha c = identifier c cs
    | isSpace c = tokenize cs
    | otherwise = error $ "Cannot tokenize " ++ [c]

lookAtNextToken :: [Token] -> Token
lookAtNextToken [] = TokenEndOfInput
lookAtNextToken (c:_) = c

acceptToken :: [Token] -> [Token]
acceptToken [] = error  "Nothing to accept"
acceptToken (_:ts) = ts
