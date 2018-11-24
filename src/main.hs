
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
           | TokenLeftParen
           | TokenRightParen
           | TokenEnd
    deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize [] = []
tokenize (x:xs)
    | isSpace x = tokenize xs
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
