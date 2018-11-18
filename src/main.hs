
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

constant :: String -> Constant
constant [] = error $ "No input"
constant (c: _ ) | c >= '0' && c <= '9' = IntegerConstant (digitToInt c)
                 | otherwise         = error $ "Unknown input"

constantToString :: Constant -> String
constantToString (IntegerConstant c) = show c
constantToString (CharacterConstant c) = [c]
constantToString (FloatingConstant c) = show c

data AssignmentOperator = AssignmentRegular
                        | AssignmentPlus
                        | AssignmentMinus
                        | AssignmentTimes
                        | AssignmentDiv
                        | AssignmentModulo
                        | AssignmentShiftLeft
                        | AssignmentShiftRight
                        | AssignmentAnd
                        | AssignmentExp
                        | AssignmentOr
        deriving (Show, Eq)

assignmentOperator :: String -> AssignmentOperator
assignmentOperator str | str == "="   = AssignmentRegular
                       | str == "+="  = AssignmentPlus
                       | str == "-="  = AssignmentMinus
                       | str == "*="  = AssignmentTimes
                       | str == "/="  = AssignmentMinus
                       | str == "%="  = AssignmentModulo
                       | str == "<<=" = AssignmentShiftLeft
                       | str == ">>=" = AssignmentShiftRight
                       | str == "&="  = AssignmentAnd
                       | str == "^="  = AssignmentExp
                       | str == "|="  = AssignmentOr
                       | otherwise    = error $ "Unknown assignment operator: " ++ str

assignmentOperatorToString :: AssignmentOperator -> String
assignmentOperatorToString ao | ao == AssignmentRegular     = "="
                              | ao == AssignmentPlus        = "+="
                              | ao == AssignmentMinus       = "-="
                              | ao == AssignmentTimes       = "*="
                              | ao == AssignmentDiv         = "/="
                              | ao == AssignmentModulo      = "%="
                              | ao == AssignmentShiftLeft   = "<<="
                              | ao == AssignmentShiftRight  = ">>="
                              | ao == AssignmentAnd         = "&="
                              | ao == AssignmentExp         = "^="
                              | ao == AssignmentOr          = "|="
                              | otherwise                   = error $ "Unknown assignment operator"

listAssignmentOperatorStrings :: [String]
listAssignmentOperatorStrings = ["=","+=","-=","*=","/=","%=","<<=",">>=","&=","^=","|="]

data Operator = OperatorPlus
              | OperatorMinus
              | OperatorTimes
              | OperatorDiv
              | OperatorModulo
              | OperatorNegation
              | OperatorLessThan
              | OperatorGreaterThan
              | OperatorAssignment AssignmentOperator
              | OperatorEqualTo
              | OperatorNotEqualTo
              | OperatorGreaterThanOrEqualTo
              | OperatorLessThanOrEqualTo
              | OperatorInclusiveOr
              | OperatorLogicalAnd
              | OperatorLogicalOr
        deriving (Show, Eq)

operator :: String -> Operator
operator str | str == "+" = OperatorPlus
             | str == "-" = OperatorMinus
             | str == "*" = OperatorTimes
             | str == "/" = OperatorDiv
             | str == "%" = OperatorModulo
             | str == "!" = OperatorNegation
             | str == "<" = OperatorLessThan
             | str == ">" = OperatorGreaterThan
             | str == "=="  = OperatorEqualTo
             | str == "!="  = OperatorNotEqualTo
             | str == ">="  = OperatorGreaterThanOrEqualTo
             | str == "<="  = OperatorLessThanOrEqualTo
             | str == "|"  = OperatorInclusiveOr
             | str == "&&"  = OperatorLogicalAnd
             | str == "||"  = OperatorLogicalOr
             | elem str listAssignmentOperatorStrings = OperatorAssignment (assignmentOperator str)
             | otherwise = error $ "Unknown operator"

operatorToString :: Operator -> String
operatorToString op | op == OperatorPlus = "+"
                    | op == OperatorMinus = "-"
                    | op == OperatorTimes = "*"
                    | op == OperatorDiv = "/"
                    | op == OperatorModulo = "%"
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

-- TODO: Implement sym function
sym :: String -> (String, String)
sym str = symbols "" str
    where
        symbols acc [] = (acc, [])
        symbols acc (c : cs)
            | isSymbol c =
                let (acc', cs') = symbols acc cs
                in (c:acc', cs')
            | otherwise = (acc, c:cs)

symbol :: Char -> String -> [Token]
symbol c cs =
    let
        (str, cs') = sym cs
    in
        TokenOperator (operator (c:str)) : tokenize cs'

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs)
    | elem c "+" = TokenOperator (operator [c]) : tokenize cs
    | isDigit c = TokenConstant (constant [c]) : tokenize cs
    | isSymbol c = symbol c cs
    | isSpace c = tokenize cs
    | otherwise = error $ "Unknown input"

main :: IO ()
main = do
    print $ constant "3"
    print $ operator "+"
    print $ tokenize "+3"
    print $ tokenize "3 + 2 + 1"
    print $ tokenize "3 = 3"
    print $ tokenize "2 == 2"
