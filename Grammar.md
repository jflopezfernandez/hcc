
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

logical-AND-expression:
    inclusive-OR-expression
    logical-AND-expression && inclusive-OR-expression

inclusive-OR-expression:
    exclusive-OR-expression
    inclusive-OR-expression | exclusive-OR-expression

exclusive-OR-expression:
    AND-expression
    exclusive-OR-expression ^ AND-expression

AND-expression:
    equality-expression
    AND-expression & equality-expression

equality-expression:
    relational-expression
    equality-expression == relational-expression
    equality-expression != relational-expression

relational-expression:
    shift-expression
    relational-expression < shift-expression
    relational-expression > shift-expression
    relational-expression <= shift-expression
    relational-expression >= shift-expression

shift-expression:
    additive-expression
    shift-expression << additive-expression
    shift-expression >> additive-expression

additive-expression:
    multiplicative-expression
    additive-expression + multiplicative-expression
    additive-expression - multiplicative-expression

multiplicative-expression:
    cast-expression
    multiplicative-expression * cast-expression
    multiplicative-expression / cast-expression
    multiplicative-expression % cast-expression

cast-expression:
    unary-expression
    ( type-name ) cast-expression

unary-expression:
    postfix-expression
    ++ unary-expression
    -- unary-expression
    unary-operator cast-expression
    sizeof unary-expression
    sizeof ( type-name )

unary-operator: one of
    & * + - ~ !

postfix-expression:
    primary-expression
    postfix-expression [ expression ]
    postfix-expression ( argument-expression-list(opt) )
    postfix-expression . identifier
    postfix-expression -> identifier
    postfix-expression ++
    postfix-expression --

primary-expression:
    identifier
    constant
    string
    ( expression )

argument-expression-list:
    assignment-expression
    argument-expression-list, assignment-expression

constant:
    integer-constant
    character-constant
    floating-constant
    enumeration-constant

```
