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
To reassign a variable do this: 
```
a = 2 - 1
```
You can also assign one variable to another.
```
let a = 2
let b = a + 3
```
This will result in an error.
### If Statements
I don't want to explain this so here's an example:
```
if 1 == 2 - 1 {
    syscall 60, 1
}
```
### Syscalls
Syscalls are currently the only built in functions. It would be possible to create nearly any function with syscalls.
To call a syscall you do this: 
```
syscall 60, 1
```
The first argument is the syscall number. All the following arguments are just the arguments for the syscall. If you don't provide an argument it is going to be 0.
You can only use numbers as arguments (Soon you will be able to use variables).
