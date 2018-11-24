
import Data.Char

data Operator = Addition
              | Subtraction
              | Multiplication
              | Division
    deriving (Show, Eq)

operator :: String -> Operator
operator op | op == "+" = Addition
            | op == "-" = Subtraction
            | op == "*" = Multiplication
            | op == "/" = Division
            | otherwise = error $ "Unknown operator: " ++ op

data Token = TokenIdentifier String
           | TokenInteger Int
           | TokenDouble Double
           | TokenBoolean Bool
           | TokenOperator Operator
           | TokenDataTypeDeclaration String
           | TokenAssignment
           | TokenLeftParen
           | TokenRightParen
           | TokenEndLine
           | TokenEnd
    deriving (Show, Eq)

listDataTypes :: [String]
listDataTypes = ["void","char","short","int","long","float","double","bool","signed","unsigned"]

classifyIdentifier :: String -> Token
classifyIdentifier "" = error "No input string"
classifyIdentifier s | elem s listDataTypes = TokenDataTypeDeclaration s
                     | elem s ["true","false"] =
                        case s of
                            "true" -> TokenBoolean True
                            _ -> TokenBoolean False
                     | otherwise = TokenIdentifier s

identifier :: String -> String -> [Token]
identifier acc "" = TokenIdentifier acc : tokenize ""
identifier acc (x:xs)
    | isAlphaNum x = identifier (acc ++ [x]) xs
    | otherwise = classifyIdentifier acc : tokenize (x:xs)

numeric :: Bool -> String -> String -> [Token]
numeric False acc "" = TokenInteger (read acc :: Int) : tokenize ""
numeric True acc "" = TokenDouble (read acc :: Double) : tokenize ""
numeric b acc (x:xs)
    | isDigit x = numeric b (acc ++ [x]) xs
    | x == '.' && b == False = numeric True (acc ++ [x]) xs
    | x == '.' && b == True = error $ "Double period in number: " ++ acc ++ [x]
    | otherwise = 
        case b of
            True ->
                TokenDouble (read acc :: Double) : tokenize (x:xs)
            False ->
                TokenInteger (read acc :: Int) : tokenize (x:xs)

tokenize :: String -> [Token]
tokenize [] = []
tokenize (x:xs)
    | isAlpha x = identifier "" (x:xs)
    | isDigit x = numeric False "" (x:xs)
    | isSpace x = tokenize xs
    | x == '=' = TokenAssignment : tokenize xs
    | x == '(' = TokenLeftParen : tokenize xs
    | x == ')' = TokenRightParen : tokenize xs
    | x == ';' = TokenEndLine : tokenize xs
    | elem x "+-*/" = TokenOperator (operator [x]) : tokenize xs
    | otherwise = TokenIdentifier [x] : tokenize xs

main :: IO ()
main = do
    line <- getLine
    if (length (line :: String)) == 0 then
        return ()
    else do
        (print . tokenize) (line :: String)
        main
