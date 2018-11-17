
module Main where

import Lexer (tokenize)
import Parser (parse)
import Evaluator (evaluate)

import System.IO
import System.Environment
import Data.Char
import qualified Data.Map as DataMap


simpleExample :: String
simpleExample = "x = 4 * 8;"

loop symbolTable = do
    str <- getLine
    if null str then
        return ()
    else
        let
            toks = tokenize str
            tree = parse toks
            (val, symbolTable') = evaluate tree symbolTable
        in
            do
                print val
                loop symbolTable'


main :: IO ()
main = do
    loop (DataMap.fromList [("pi", pi),("e",exp 1.0)])

    --(print . parse . tokenize) simpleExample
    --(print . evaluate . parse . tokenize) simpleExample
    
    --(print . parse . tokenize) "int x = 4 * 8;"

    --(print . evaluate . parse . tokenize) "x1 = -15 / (2 + x2)"
    --(print . evaluate . parse . tokenize) "x + 3 = 7"
    --(print . evaluate . parse . tokenize) "int x = 1 + 4;"

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
