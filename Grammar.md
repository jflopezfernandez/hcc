
# C Language Grammar

```
expression:
    assignment-expression
    expression, assignment-expression

assignment-expression:
    conditional-expression
    unary-expression assignment-operator assignment-expression

assignment-operator: one of
    = *= /= %= += -= <<= >>= &= ^= |=

conditional-expression:
    logical-OR-expression
    logical-OR-expression ? expression : conditional-expression

constant-expression:
    conditional-expression

logical-OR-expression:
    logical-AND-expression
    logical-OR-expression || logical-AND-expression
```
