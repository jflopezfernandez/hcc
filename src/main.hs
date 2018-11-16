
module Main where

import System.IO
import System.Environment
import Data.Char
-- import Control.Monad
-- import Text.Parsec
-- import Text.Parsec.Expr
-- import Text.Parsec.Language
-- import qualified Text.Parsec.Token as Token

data Operator = Plus
              | Minus
              | Times
              | Div
              | Modulo
              | Negation
              | Exponentiation
              | LessThan
              | GreaterThan
        deriving (Show, Eq)

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Times
           | c == '/' = Div
           | c == '%' = Modulo
           | c == '!' = Negation
           | c == '^' = Exponentiation
           | c == '<' = LessThan
           | c == '>' = GreaterThan
           | otherwise = error $ "Unknown operator input: " ++ [c]

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

--data Expression

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
    | elem c "+-/*/%!^" = TokenOperator (operator c) : tokenize cs
    | c == ';' = TokenEndOfLine : tokenize cs
    | c == '(' = TokenLeftParen : tokenize cs
    | c == ')' = TokenRightParen : tokenize cs
    | c == '=' = TokenAssignment : tokenize cs
    | c == '#' = directive c cs
    | isDigit c = number c cs
    | isAlpha c = identifier c cs
    | isSpace c = tokenize cs
    | otherwise = error $ "Cannot tokenize " ++ [c]

data Tree = VariableNode String
          | NumberNode Double
          | AssignmentNode String Tree
          | ProductNode Operator Tree Tree
          | SumNode Operator Tree Tree
          | UnaryNode Operator Tree
        deriving (Show, Eq)

expression :: [Token] -> (Tree, [Token])
expression toks =
    let
        (termTree, toks') = term toks
    in
        case lookAtNextToken toks' of
            (TokenOperator op) | elem op [Plus, Minus] ->
                let (exTree, toks'') = expression (acceptToken toks')
                in (SumNode op termTree exTree, toks'')
            TokenAssignment ->
                case termTree of
                    VariableNode str ->
                        let (exTree, toks'') = expression (acceptToken toks')
                        in (AssignmentNode str exTree, toks'')
                    _ -> error $ "Only variables can be assigned to."
            _ -> (termTree, toks')

term :: [Token] -> (Tree, [Token])
term toks =
    let
        (facTree, toks') = factor toks
    in
        case lookAtNextToken toks' of
            (TokenOperator op) | elem op [Times, Div] ->
                let (termTree, toks'') = term (acceptToken toks')
                in (ProductNode op facTree termTree, toks'')
            _ -> (facTree, toks')

factor :: [Token] -> (Tree, [Token])
factor toks =
    case lookAtNextToken toks of
        (TokenNumber n) -> (NumberNode n, acceptToken toks)
        (TokenIdentifier str) -> (VariableNode str, acceptToken toks)
        (TokenOperator op) | elem op [Plus, Minus] ->
            let (facTree, toks') = factor (acceptToken toks)
            in (UnaryNode op facTree, toks')
        TokenLeftParen ->
            let (expTree, toks') = expression (acceptToken toks)
            in
                if lookAtNextToken toks' /= TokenRightParen then
                    error "Missing right parenthesis"
                else
                    (expTree, acceptToken toks')
        _ -> error $ "Parse error on token: " ++ show toks

lookAtNextToken :: [Token] -> Token
lookAtNextToken [] = TokenEndOfInput
lookAtNextToken (c:_) = c

acceptToken :: [Token] -> [Token]
acceptToken [] = error  "Nothing to accept"
acceptToken (_:ts) = ts

parse :: [Token] -> Tree
parse toks =
    let
        (tree, toks') = expression toks
    in
        if null toks' then
            tree
        else
            error $ "Leftover tokens: " ++ show toks'

evaluate :: Tree -> Double
evaluate (SumNode op left right) =
    let 
        lft = evaluate left
        rgt = evaluate right
    in
        case op of
            Plus -> lft + rgt
            Minus -> lft - rgt

evaluate (ProductNode op left right) =
    let
        lft = evaluate left
        rgt = evaluate right
    in
        case op of
            Times -> lft * rgt
            Div -> lft / rgt

evaluate (UnaryNode op tree) =
    let
        x = evaluate tree
    in
        case op of
            Plus -> x
            Minus -> (-x)

evaluate (NumberNode x) = x

-- Dummy Implementation
evaluate (AssignmentNode str tree) = evaluate tree

-- Dummy Implementation
evaluate (VariableNode str) = 0




main :: IO ()
main = (print . evaluate . parse. tokenize) "x1 = -15 / (2 + x2)"

    -- print $ tokenize "double result = 1 + 4 / x;"
    -- print $ tokenize "bool not_x = !x;"
    -- print $ tokenize "float y=8^2;"
    -- print $ tokenize "float y = 3x/2 + 7;"
    -- print $ tokenize "double y = ln x;"
    -- print $ tokenize "x = 1;"
    -- print $ tokenize "int x = 3 * 5;"
    -- print $ tokenize "#include iostream"
    -- print $ alnums "main()"
    -- print $ tokenize "void printSomething();"

--  AssignmentNode "x1"
--      (ProductNode Div
--          (UnaryNode Minus
--              (NumberNode 15.0))
--          (SumNode Plus
--              (NumberNode 2.0)
--              (VariableNode "x2")))
