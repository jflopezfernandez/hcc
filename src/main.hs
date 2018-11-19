
module Main where

--import Lexer (tokenize)
--import Parser (parse)
--import Evaluator (evaluate)

-- import System.IO
-- import System.Environment
import Data.Char
import Data.DeriveTH
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

isIntegerConstant :: Constant -> Bool
isIntegerConstant (IntegerConstant _) = True
isIntegerConstant _ = False

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

-- TODO: In order to be able to handle pointer declarations and dereferencing,
-- the handling of the '*' character will have to be made more robust.

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

-- Function: buildIdentifierToken
--
-- Description: This function takes the integer value from "idmatcher" as well
-- as the string from "identifier" and calls the token constructor function
-- based on the integer i.
--
-- 1: TokenStorageClassSpecifier
-- 2: TokenTypeQualifier
--
-- If the value of i isn't explicitly listed (the default return of "idmatcher"
-- is 0), then it simply constructs a generic TokenIdentifier object, which will
-- usually represent variables, function names, etc.

buildIdentifierToken :: Int -> String -> Token
buildIdentifierToken i str | i == 1     = TokenTypeQualifier (typeQualifier str)
                           | i == 2     = TokenStorageClassSpecifier (storageClassSpecifier str)
                           | otherwise  = TokenIdentifier str

-- Function: idmatcher
--
-- Description: Since there are several types of identifiers, ranging from user-
-- specified identifiers to storage class specifiers and type qualifiers, I 
-- wrote this function to classify the identifier type by searching for whether
-- it exists in any of the predefined keyword lists. If it does, the function
-- returns an integer value corresponding to the type of identifier it is, and
-- the calling function "identifier" subsequently calls "buildIdentifierToken"
-- to actually construct the Token.

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

data Tree = ConstantNode Constant
          | IdentifierNode String
          | UnaryExpressionNode Operator Tree
          | AdditiveExpressionNode Operator Tree Tree
          | MultiplicativeExpressionNode Operator Tree Tree
          | AssignmentNode String Tree
        deriving (Show, Eq)

lookAtNextToken :: [Token] -> Token
lookAtNextToken [] = TokenEndOfInput
lookAtNextToken (c:_) = c

acceptToken :: [Token] -> [Token]
acceptToken [] = error $ "Nothing to accept"
acceptToken (_:ts) = ts

expression :: [Token] -> (Tree, [Token])
expression tokens = 
    let
        (termTree, tokens') = term tokens
    in
        case lookAtNextToken tokens' of
            (TokenOperator op) | elem op [OperatorPlus, OperatorMinus] ->
                let
                    (exTree, tokens'') = expression (acceptToken tokens')
                in
                    (AdditiveExpressionNode op termTree exTree, tokens'')
            
            (TokenOperator op) | op == TokenOperator ->
                case termTree of
                    IdentifierNode str ->
                        let (exTree, tokens'') = expression (acceptToken tokens')
                        in (AssignmentNode str exTree, tokens'')
                    
                    _ -> error $ "Only lvalues may be assigned to"
            
            _ -> (termTree, tokens')

term :: [Token] -> (Tree, [Token])
term tokens = 
    let
        (factorTree, tokens') = factor tokens
    in
        case lookAtNextToken tokens' of
            (TokenOperator op) | elem op [OperatorTimes, OperatorDiv] ->
                let
                    (termTree, tokens'') = term (acceptToken tokens')
                in
                    (MultiplicativeExpressionNode op factorTree termTree, tokens'')
            
            _ -> (factorTree, tokens')

factor :: [Token] -> (Tree, [Token])
factor tokens =
    case lookAtNextToken tokens of
        (TokenConstant c) -> (ConstantNode c, acceptToken tokens)

        (TokenIdentifier str) -> (IdentifierNode str, acceptToken tokens)

        (TokenOperator op) | elem op [OperatorPlus, OperatorMinus] ->
            let
                (factorTree, tokens') = factor (acceptToken tokens)
            in
                (UnaryExpressionNode op factorTree, tokens')

        TokenLeftParen ->
            let
                (expTree, tokens') = expression (acceptToken tokens)
            in
                if lookAtNextToken tokens' /= TokenRightParen then
                    error $ "Missing right parenthesis"
                else
                    (expTree, acceptToken tokens')
        
        _ -> error $ "Parse error on token: " ++ show tokens

parse :: [Token] -> Tree
parse tokens =
    let
        (tree, tokens') = expression tokens
    in
        if null tokens' then
            tree
        else
            error $ "Leftover tokens: " ++ show tokens

main :: IO ()
main = do
    (print . parse . tokenize) "a"
    (print . parse . tokenize) "15"
    (print . parse . tokenize) "42 + 4"
    (print . parse . tokenize) "408 - a"
    (print . parse . tokenize) "-b"
    (print . parse . tokenize) "a * 4"
    (print . parse . tokenize) "a * (b + c)"
    (print . tokenize) "a = 4 + b"
    (print . parse . tokenize) "a = 4 + b"
    --(print . parse . tokenize) "a = -b" -- Error
