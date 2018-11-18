
module Main where

--import Lexer (tokenize)
--import Parser (parse)
--import Evaluator (evaluate)

-- import System.IO
-- import System.Environment
import Data.Char
-- import qualified Data.Map as DataMap

-- TODO: Constant constructor can only handle single-character integers
-- TODO: Add enumeration constants
data Constant = IntegerConstant Int
              | CharacterConstant Char
              | FloatingConstant Double
        deriving (Show, Eq)

constant :: Char -> Constant
constant c | c >= '0' && c <= '9' = IntegerConstant (digitToInt c)
           | otherwise         = error $ "Unknown input"

constantToString :: Constant -> String
constantToString (IntegerConstant c) = show c
constantToString (CharacterConstant c) = [c]
constantToString (FloatingConstant c) = show c

data Operator = OperatorPlus
              | OperatorMinus
              | OperatorTimes
              | OperatorDiv
              | OperatorModulo
        deriving (Show, Eq)

operator :: String -> Operator
operator str | str == "+" = OperatorPlus
             | otherwise = error $ "Unknown operator"

operatorToString :: Operator -> String
operatorToString op | op == OperatorPlus = "+"
                    | otherwise = error $ "Unknown operator"

data Token = TokenIdentifier String
           | TokenConstant Constant
           | TokenOperator Operator
        deriving (Show, Eq)

token :: Token
token = TokenIdentifier "a"

showTokenContent :: Token -> String
showTokenContent (TokenIdentifier str) = str
showTokenContent (TokenConstant c) = constantToString c
showTokenContent (TokenOperator op) = operatorToString op

tokenize :: String -> [Token]
tokenize = undefined

main :: IO ()
main = do
    print $ constant '3'
    print $ operator "+"
