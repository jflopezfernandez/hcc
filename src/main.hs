
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
              | Assignment
        deriving (Show, Eq)

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Times
           | c == '/' = Div
           | c == '%' = Modulo
           | c == '!' = Negation
           | c == '^' = Exponentiation
           | c == '=' = Assignment
           | otherwise = error $ "Unknown operator input: " ++ [c]

operatorToString :: Operator -> String
operatorToString Plus           = "+"
operatorToString Minus          = "-"
operatorToString Times          = "*"
operatorToString Div            = "/"
operatorToString Modulo         = "%"
operatorToString Negation       = "!"
operatorToString Exponentiation = "^"
operatorToString Assignment     = "="

data Token = TokenOperator Operator
           | TokenIdentifier String
           | TokenNumber Int
        deriving (Show, Eq)

showOperatorContent :: Token -> String
showOperatorContent (TokenOperator op) = operatorToString op
showOperatorContent (TokenIdentifier str) = str
showOperatorContent (TokenNumber n) = show n

data Expression

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:cs)
    | elem c "+/*/%!^=" = TokenOperator (operator c) : tokenize cs
    | isDigit c = TokenNumber (digitToInt c) : tokenize cs
    | isAlpha c = TokenIdentifier [c] : tokenize cs
    | isSpace c = tokenize cs
    | otherwise = error $ "Cannot tokenize " ++ [c]

parse :: [Token] -> Expression
parse = undefined

evaluate :: Expression -> Double
evaluate = undefined

--

main :: IO ()
main = do
    print $ tokenize " 1 + 4 / x"
    print $ tokenize "!x"
    print $ tokenize "y=8^2"
    print $ tokenize "y = 3x/2 + 7"
    print $ tokenize "y = ln x"
