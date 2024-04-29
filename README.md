# u-lang

## wip
This is a work in progess. Please don't use this for something serious.

## Features
### Exiting with the last expression
One feature right now is exiting with the error code of the last statement. This will get changed later when something like exit 1 will be implemented.

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

#### Variables
##### Declarations
You can define variables with the keyword let.
Here's an example:
```
let a = 2
```
To reasign a variable to this: 
```
a = 2 - 1
```
Right now you cannot assign a variable to another.
```
let a = 2
let b = a
```
This will result in an error.
