@ECHO OFF

CLS

ghc -o hcc.exe -g -Werror main.hs Lexer.hs Parser.hs Evaluator.hs
