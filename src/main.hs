
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
           | TokenOperator Operator
           | TokenAssignment
           | TokenLeftParen
           | TokenRightParen
           | TokenEnd
    deriving (Show, Eq)

listDataTypes :: [String]
listDataTypes = ["void","char","short","int","long","float","double","bool","signed","unsigned"]

identifier :: String -> String -> [Token]
identifier acc "" = TokenIdentifier acc : tokenize ""
identifier acc (x:xs)
    | isAlphaNum x = identifier (acc ++ [x]) xs
    | otherwise = TokenIdentifier acc : tokenize (x:xs)

numeric :: String -> String -> [Token]
numeric acc "" = TokenInteger (read acc) : tokenize ""
numeric acc (x:xs)
    | isDigit x = numeric (acc ++ [x]) xs
    | otherwise = TokenInteger (read acc) : tokenize xs

tokenize :: String -> [Token]
tokenize [] = []
tokenize (x:xs)
    | isAlpha x = identifier "" (x:xs)
    | isDigit x = numeric "" (x:xs)
    | isSpace x = tokenize xs
    | x == '=' = TokenAssignment : tokenize xs
    | x == '(' = TokenLeftParen : tokenize xs
    | x == ')' = TokenRightParen : tokenize xs
    | elem x "+-*/" = TokenOperator (operator [x]) : tokenize xs
    | isDigit x = TokenInteger (digitToInt x) : tokenize xs
    | otherwise = TokenIdentifier [x] : tokenize xs

main :: IO ()
main = do
    line <- getLine
    if (length (line :: String)) == 0 then
        return ()
    else do
        (print . tokenize) (line :: String)
        main
