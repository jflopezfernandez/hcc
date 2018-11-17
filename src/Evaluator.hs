
module Evaluator (evaluate) where

import Lexer
import Parser
import Control.Applicative
import Control.Monad
import qualified Data.Map as DataMap

type SymbolTable = DataMap.Map String Double

newtype Evaluator a = Ev (Either String a)

instance Monad Evaluator where
    (Ev ev) >>= k =
        case ev of
            Left msg -> Ev (Left msg)
            Right v -> k v
    return v = Ev (Right v)
    fail msg = Ev (Left msg)

--instance Functor Evaluator where
--    fmap

lookUp :: String -> SymbolTable -> (Double, SymbolTable)
lookUp str symbolTable =
    case DataMap.lookup str symbolTable of
        Just v -> (v, symbolTable)
        Nothing -> error $ "Undefined variable: " ++ str

addSymbol :: String -> Double -> SymbolTable -> Either String ((), SymbolTable)
addSymbol str val symbolTable =
    let
        symbolTable' = DataMap.insert str val symbolTable
    in
        ((), symbolTable')

evaluate :: Tree -> SymbolTable -> Evaluator (Double, SymbolTable)

{- 
evaluate (SumNode op left right) symbolTable =
    evaluate left symbolTable >>= \(lft, symbolTable') ->
        evaluate right symbolTable' >>= \(rgt, symbolTable'') ->
            case op of
                Plus ->  return (lft + rgt, symbolTable'')
                Minus -> return (lft - rgt, symbolTable'')

Equivalent to the code below.

-}

evaluate (SumNode op left right) symbolTable = do
    (lft, symbolTable')  <- evaluate left symbolTable
    (rgt, symbolTable'') <- evaluate right symbolTable'
    case op of
        Plus  -> return (lft + rgt, symbolTable'')
        Minus -> return (lft - rgt, symbolTable'')

evaluate (ProductNode op left right) symbolTable =
    let
        (lft, symbolTable') = evaluate left symbolTable
        (rgt, symbolTable'') = evaluate right symbolTable'
    in
        case op of
            Times -> (lft * rgt, symbolTable)
            Div -> (lft / rgt, symbolTable)

evaluate (UnaryNode op tree) symbolTable =
    let
        (x, symbolTable') = evaluate tree symbolTable
    in
        case op of
            Plus -> (x, symbolTable')
            Minus -> ((-x), symbolTable')
            Negation -> ((-x), symbolTable')

evaluate (NumberNode x) symbolTable = (x, symbolTable)

evaluate (VariableNode str) symbolTable = lookUp str symbolTable

evaluate (AssignmentNode str tree) symbolTable =
    let
        (v, symbolTable')  = evaluate tree symbolTable
        (_, symbolTable'') = addSymbol str v symbolTable'
    in
        (v, symbolTable'')
    
-- evaluate (DeclarationNode t tree) symbolTable =
--     let
--         (t, symbolTable') = evaluate tree symbolTable
--     in
--         case t of
--             TypeInt -> (t + 1000, symbolTable')
--             _ -> (13004, symbolTable')
