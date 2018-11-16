
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
        deriving (Show, Eq)

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Times
           | c == '/' = Div
           | c == '%' = Modulo
           | c == '!' = Negation
           | c == '^' = Exponentiation
           | otherwise = error $ "Unknown operator input: " ++ [c]

operatorToString :: Operator -> String
operatorToString Plus           = "+"
operatorToString Minus          = "-"
operatorToString Times          = "*"
operatorToString Div            = "/"
operatorToString Modulo         = "%"
operatorToString Negation       = "!"
operatorToString Exponentiation = "^"

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
    
data Token = TokenOperator Operator
           | TokenIdentifier String
           | TokenDataType DataType
           | TokenNumber Int
           | TokenLeftParen
           | TokenRightParen
           | TokenEndOfLine
           | TokenAssignment
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

--data Expression

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
        if elem (c:str) dataTypesList then
            TokenDataType (datatype (c:str)) : tokenize cs'
        else
            TokenIdentifier (c:str) : tokenize cs'

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:cs)
    | elem c "+/*/%!^()" = TokenOperator (operator c) : tokenize cs
    | c == ';' = TokenEndOfLine : tokenize cs
    | c == '(' = TokenLeftParen : tokenize cs
    | c == ')' = TokenRightParen : tokenize cs
    | c == '=' = TokenAssignment : tokenize cs
    | isDigit c = TokenNumber (digitToInt c) : tokenize cs
    | isAlpha c = identifier c cs
    | isSpace c = tokenize cs
    | otherwise = error $ "Cannot tokenize " ++ [c]

--parse :: [Token] -> Expression
--parse = undefined

--evaluate :: Expression -> Double
--evaluate = undefined

--

main :: IO ()
main = do
    print $ tokenize "double result = 1 + 4 / x;"
    print $ tokenize "bool not_x = !x;"
    print $ tokenize "float y=8^2;"
    print $ tokenize "float y = 3x/2 + 7;"
    print $ tokenize "double y = ln x;"
    print $ tokenize "x = 1;"
    print $ tokenize "int x = 3 * 5;"
