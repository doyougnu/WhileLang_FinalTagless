# FinallyTagless
# WhileLang_FinalTagless

## What
This project's purpose is to implement the WHILE language in final tagless style

## Code Descriptions

### BoolExpr Class
This class defines operations that are valid for Boolean values

### ArExpr Class
This class defines operations that are valid for arithmetic values. 

### Stmt Class
This class defines operations that are valid statements in the while language. 
These are such things as let statments, variable lookup and control flow
statements such as if and while.

### Data Types
I define three data types for this project.

1. The first is Prims, these are primitive values in the language and are used
   as values in the state mapping.
   
2. The second, VarStore, is a type synonym for a variable store. I leave this
   type polymorphic for reuse. This is just a map with strings for keys and the
   type argument as values

3. The third, Eval, is essentially a state monad with a phantom type to satisfy
   the final tagless style. I force the values to be Prims in the state so that
   the language can be an instance of classes that return different types. For
   example, if I leave the values polymorphic then I cannot make Eval an
   instance of an ArExpr class because I have no way to constrain the internal
   types to Integers and then pass them to the eq operator.

## Other Comments
The rest of the code is just some testing expressions and the instantiation of 
Eval for these type classes. Almost all of the instantiations are defined in the
nitty gritty details of the state monad. Time permitting, I'll clean it up.

## This roughly implements the AST below

``` haskell
type Var = String

-- *  Booleans
-- | Binary Boolean Ops
data BinBoolOp = And | Or
               deriving (Eq, Show, Ord)

-- | Relational Boolean Ops
data RelBoolOp = Less
               | Greater
               | Equal
               | NEqual
               deriving (Eq, Show, Ord)

-- | Boolean Expressions
data BoolExpr = B Bool
              | Not BoolExpr
              | BBinary BinBoolOp BoolExpr BoolExpr
              | RBinary RelBoolOp ArExpr ArExpr
              | BNoOp
              deriving (Eq, Show, Ord)

-- * Arithmetic Operators
-- | Binary Arithmatic operators
data ArBinOp = Add
             | Subtract
             | Multiply
             | Divide
             deriving (Eq, Show, Ord)

-- | Arithmetic Expressions
data ArExpr  = V Var
            | I Integer
            | Neg ArExpr
            | ABinary ArBinOp ArExpr ArExpr
            | ArNoOp
            deriving (Eq, Show, Ord)

-- * Statements
data Stmt = BL BoolExpr
          | AR ArExpr
          | ST String
          | Let String Stmt
          | If BoolExpr Stmt  Stmt
          | While BoolExpr Stmt
          | Seq [Stmt]
          | NoOp
          deriving (Eq, Show, Ord)
```
