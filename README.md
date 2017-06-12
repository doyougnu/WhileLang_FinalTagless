# WhileLang_FinalTagless

## What
This project's purpose is to implement the WHILE language in final tagless style

## Where
If you're confused about where the code is, just look at src/

## How to run
I am expecting anyone who wants to use this to be familiar with ghc and ghci. So
in order to run anything in the src/Lang.hs file just boot up ghci in the root
folder with the command ```stack ghci``` and you'll enter the right REPL. Then
to run any statement you may want to run you'll need to pass it to the runEval
function, for the core language, and runNEval for the extended language. Here
are some examples, you can find more at the bottom of the Extensions.hs file:

### Premade Examples:

1. ```runEval ifTest emptyState```
2. ```runEval letTest emptyState```
3. ```runEval whileTest  emptyState```
4. ```runEval seqTest emptyState```

### Running your own examples
If you're going to try to write a program yourself you'll need to specify a type
in order to evaluate. Like this:

```haskell
-- say we want something like this
let x = if_ (eq (lit 1) (lit 2)) (add (lit 3) (lit 10)) (neg (lit 100))
-- obviously this will be returning an Int. Thus the final expression should be:
x :: Eval Int
let x = if_ (eq (lit 1) (lit 2)) (add (lit 3) (lit 10)) (neg (lit 100))

-- Now to run it, we know we don't depend on anything variables in the state so:
-- the first element of the tuple is the global state, the snd the result
λ> runEval x emptyState
   (fromList [("",NoOp)],I (-100))

-- If you do need variables make sure they are defined or in the initial state
y :: Eval Bool
let y = while ((var "cond")) (let_ "cond" fls)

-- we are referring to the variable "cond" without defining it so make sure it is
-- in the initial environment or else you'll get an exception
λ> runEval y $ M.singleton "cond" (B False)
   (fromList [("cond",B False)],NoOp)
```
