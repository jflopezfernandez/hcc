
module Parser (Tree(..), parse) where

import Lexer

data Tree = VariableNode String
          | NumberNode Double
          | AssignmentNode String Tree
          | ProductNode Operator Tree Tree
          | SumNode Operator Tree Tree
          | UnaryNode Operator Tree
          | DeclarationNode DataType Tree
        deriving (Show, Eq)

parse :: [Token] -> Tree
parse toks =
    let
        (tree, toks') = expression toks
    in
        if null toks' then
            tree
        else
            if elem (lookAtNextToken toks') [TokenEndOfLine] then
                tree
            else
                error $ "Leftover tokens: " ++ show toks'

expression :: [Token] -> (Tree, [Token])
expression toks =
    let
        (termTree, toks') = term toks
    in
        case lookAtNextToken toks' of
            (TokenOperator op) | elem op [Plus, Minus] ->
                let (exTree, toks'') = expression (acceptToken toks')
                in (SumNode op termTree exTree, toks'')
            TokenAssignment ->
                case termTree of
                    VariableNode str ->
                        let (exTree, toks'') = expression (acceptToken toks')
                        in (AssignmentNode str exTree, toks'')
                    _ -> error $ "Only variables can be assigned to."
            _ -> (termTree, toks')

term :: [Token] -> (Tree, [Token])
term toks =
    let
        (facTree, toks') = factor toks
    in
        case lookAtNextToken toks' of
            (TokenOperator op) | elem op [Times, Div] ->
                let (termTree, toks'') = term (acceptToken toks')
                in (ProductNode op facTree termTree, toks'')
            _ -> (facTree, toks')

factor :: [Token] -> (Tree, [Token])
factor toks =
    case lookAtNextToken toks of
        (TokenNumber n) -> (NumberNode n, acceptToken toks)
        (TokenIdentifier str) -> (VariableNode str, acceptToken toks)
        (TokenOperator op) | elem op [Plus, Minus, Negation] ->
            let
                (facTree, toks') = factor (acceptToken toks)
            in
                (UnaryNode op facTree, toks')
        TokenLeftParen ->
            let
                (expTree, toks') = expression (acceptToken toks)
            in
                if lookAtNextToken toks' /= TokenRightParen then
                    error "Missing right parenthesis"
                else
                    (expTree, acceptToken toks')
        _ -> error $ "Parse error on token: " ++ show toks
