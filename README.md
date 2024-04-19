# u-lang

## wip
This is a work in progess. Please don't use this for something serious.

## Features
The only feature right now is exiting with the error code of the last statement.

Statements can only be of type expression. And an expression can only be either be a NumberExpression or a BinaryExpressoin. BinaryExpression can either be plus or minus and only one.
So no:
```
1 + 2 + 3
```
Only:
```
1 + 3
3 + 5
```
The second one would exit with the code: 8.



