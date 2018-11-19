
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
constant (c : cs) = IntegerConstant (read (c:cs))
    -- | c >= '0' && c <= '9' = IntegerConstant (digitToInt c)
    -- | otherwise         = error $ "Unknown input"

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

listOperators :: [String]
listOperators = ["+","-","*","/","%","!","<",">","==","!=",">=","<=","|","&&","||"]

-- TODO: Implement struct-or-union-specifier and enum-specifier
data TypeQualifier = TypeVoid
                   | TypeChar
                   | TypeShort
                   | TypeInt
                   | TypeLong
                   | TypeFloat
                   | TypeDouble
                   | TypeBoolean
                   | TypeSigned
                   | TypeUnsigned
        deriving (Show, Eq)

typeQualifier :: String -> TypeQualifier
typeQualifier str | str == "void"     = TypeVoid
                  | str == "char"     = TypeChar
                  | str == "short"    = TypeShort
                  | str == "int"      = TypeInt
                  | str == "long"     = TypeLong
                  | str == "float"    = TypeFloat
                  | str == "double"   = TypeDouble
                  | str == "bool"     = TypeBoolean
                  | str == "signed"   = TypeSigned
                  | str == "unsigned" = TypeUnsigned
                  | otherwise         = error $ "Unknown type qualifier: " ++ str

typeQualifierToString :: TypeQualifier -> String
typeQualifierToString t | t == TypeVoid = "void"
                        | t == TypeChar = "char"
                        | t == TypeShort = "short"
                        | t == TypeInt = "int"
                        | t == TypeLong = "long"
                        | t == TypeFloat = "float"
                        | t == TypeDouble = "double"
                        | t == TypeBoolean = "bool"
                        | t == TypeSigned = "signed"
                        | t == TypeUnsigned = "unsigned"
                        | otherwise = error $ "Unknown type qualifier"

listTypeQualifiers :: [String]
listTypeQualifiers = ["void","char","short","int","long","float","double","bool","signed","unsigned"]

data StorageClassSpecifier = StorageClassAuto
                           | StorageClassRegister
                           | StorageClassStatic
                           | StorageClassExtern
                           | StorageClassTypedef
        deriving (Show, Eq)

storageClassSpecifier :: String -> StorageClassSpecifier
storageClassSpecifier str | str == "auto"       = StorageClassAuto
                          | str == "register"   = StorageClassRegister
                          | str == "static"     = StorageClassStatic
                          | str == "extern"     = StorageClassExtern
                          | str == "typedef"    = StorageClassTypedef
                          | otherwise           = error $ "Storage class specifier unknown: " ++ str

storageClassSpecifierToString :: StorageClassSpecifier -> String
storageClassSpecifierToString str | str == StorageClassAuto     = "auto"
                                  | str == StorageClassRegister = "register"
                                  | str == StorageClassStatic   = "static"
                                  | str == StorageClassExtern   = "extern"
                                  | str == StorageClassTypedef  = "typedef"
                                  | otherwise                   = error $ "Storage class specifier unknown"

listStorageTypeSpecifiers :: [String]
listStorageTypeSpecifiers = ["auto","register","static","extern","typedef"]

data Token = TokenIdentifier String
           | TokenConstant Constant
           | TokenOperator Operator
           | TokenTypeQualifier TypeQualifier
           | TokenStorageClassSpecifier StorageClassSpecifier
           | TokenLeftParen
           | TokenRightParen
           | TokenLeftBracket
           | TokenRightBracket
           | TokenEndOfLine
           | TokenEndOfInput
        deriving (Show, Eq)

showTokenContent :: Token -> String
showTokenContent (TokenIdentifier str) = str
showTokenContent (TokenConstant c) = constantToString c
showTokenContent (TokenOperator op) = operatorToString op
showTokenContent (TokenTypeQualifier t) = typeQualifierToString t
showTokenContent (TokenStorageClassSpecifier s) = storageClassSpecifierToString s
showTokenContent TokenLeftParen = "("
showTokenContent TokenRightParen = ")"
showTokenContent TokenLeftBracket = "{"
showTokenContent TokenRightBracket = "}"
showTokenContent TokenEndOfLine = show ";"
showTokenContent TokenEndOfInput = show "[End of Input.]"

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

-- TODO: Implement double function
--double :: String -> String -> (String, String)
--double c cs = 

-- TODO: Add type double functionality
digits :: String -> (String, String)
digits str = digs "" str
    where
        digs :: String -> String -> (String, String)
        digs acc [] = (acc, [])
        digs acc (c : cs)
            | isDigit c = let (acc', cs') = digs acc cs
                           in (c:acc', cs')
            -- | c == '.' = 
            | otherwise = (acc, c:cs)

number :: Char -> String -> [Token]
number c cs =
    let (digs, cs') = digits cs
    in TokenConstant (constant (c:digs)) : tokenize cs'

alnums :: String -> (String, String)
alnums str = als "" str
    where
        als acc [] = (acc, [])
        als acc (c:cs)
            | isAlphaNum c =
                let (acc', cs') = als acc cs
                in (c:acc', cs')
            | otherwise = (acc, c:cs)

buildIdentifierToken :: Int -> String -> Token
buildIdentifierToken i str | i == 1     = TokenTypeQualifier (typeQualifier str)
                           | i == 2     = TokenStorageClassSpecifier (storageClassSpecifier str)
                           | otherwise  = TokenIdentifier str

idmatcher :: String -> Int
idmatcher str | elem str listTypeQualifiers = 1
              | elem str listStorageTypeSpecifiers = 2
              | otherwise = 0

identifier :: Char -> String -> [Token]
identifier c cs =
    let
        (str, cs') = alnums cs
    in
        buildIdentifierToken (idmatcher (c:str)) (c:str) : tokenize cs'
        -- if elem (c:str) listTypeQualifiers then
        --     TokenTypeQualifier (typeQualifier (c:str)) : tokenize cs'
        -- else
        --     TokenIdentifier (c:str) : tokenize cs'

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs)
    | isAlpha c = identifier c cs
    | isDigit c = number c cs
    | elem c "+-*/%!=&|<>^" = symbol c cs
    | isSpace c = tokenize cs
    | c == '(' = TokenLeftParen : tokenize cs
    | c == ')' = TokenRightParen : tokenize cs
    | c == '{' = TokenLeftBracket : tokenize cs
    | c == '}' = TokenRightBracket : tokenize cs
    | c == ';' = TokenEndOfLine : tokenize cs
    | otherwise = error $ "Unknown input: " ++ [c]

data Tree = VariableNode String
          | IntegerNode Int
          | AssignmentNode
        deriving (Show, Eq)

lookAtNextToken :: [Token] -> Token
lookAtNextToken [] = TokenEndOfInput
lookAtNextToken (c:_) = c

acceptToken :: [Token] -> [Token]
acceptToken [] = error $ "Nothing to accept"
acceptToken (_:ts) = ts

expression :: [Token] -> (Tree, [Token])
expression = undefined

term :: [Token] -> (Tree, [Token])
term = undefined

factor :: [Token] -> (Tree, [Token])
factor = undefined

main :: IO ()
main = do
    print $ tokenize "int a = 300;"
    print $ tokenize "int b = (4 * a);"
    print $ tokenize "int main() { return 0; }"
    print $ tokenize "static int x = 1;"
    print $ tokenize "extern void data();"
