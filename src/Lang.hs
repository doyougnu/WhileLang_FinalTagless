{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Lang where

import Control.Monad.State
import qualified Data.Map as M
import Prelude hiding (seq)

import CoreLang
{-
Goal: Implement the WHILE language in Final Tagless form, then extend it by
adding function calls

Some Initial Insights:
1) I'm forced to strictly evaluate my language because I need to use a
homogenous map and have no handle on a Stmt ADT that I can use in the map, thus
I need to evaluate everything to a Primitive.

2) Because of 1, I can easily extend operations but If i need to extend my
primitives then I'll get into trouble because I'll need to remake my state
monad. This is still extensible because of the typeclasses but is less than
desirable.

A Disclaimer:
Ye of little faith beware, this file presupposes that you understand haskell
typeclasses, state monads and newtype record declarations. I make heavy use of
an unwrapped state monad so the functions will be confusing if you are not
familiar with the "under the hood" parts of monads.
-}

--------------------------------------------------------------------------------
-- Core Language
--------------------------------------------------------------------------------
-- | Primitive values that this language can use
data Prims = I Int | B Bool | NoOp
  deriving (Eq, Show, Ord)

-- | Map to hold let bound variables
type VarStore a = M.Map String a

-- | an eval monad, without all the sugar
newtype Eval a = Eval {runEval :: VarStore Prims -> (VarStore Prims, Prims)}

emptyState :: VarStore Prims
emptyState = M.singleton "" NoOp

-- | Bool instances
instance BoolExpr Eval where
  tru = Eval $ \s -> (s, B True)
  fls = Eval $ \s -> (s, B False)
  bEq (Eval j) (Eval j') = Eval $ \s ->
    let (s1, r1) = j s
        (s2, r2) = j' s
    in (s1 `mappend` s2, B $ r1 == r2)
  bnot (Eval j) = Eval $ \x ->
    let (s1, B r) = j x
    in (s1, B $ not r)

-- | Arithmetic instances
instance ArExpr Eval where
  lit i = Eval $ \s -> (s, I i)
  neg (Eval j) = Eval $ \s ->
    let (s', I r) = j s
    in (s', I $ negate r)

  add (Eval j) (Eval k) = Eval $ \s ->
    let (s1, I r1) = j s
        (s2, I r2) = k s
    in (s1 `mappend` s2, I $ r1 + r2)

  sub x y = add x $ neg y

  mul (Eval j) (Eval k) = Eval $ \s ->
    let (s1, I r1) = j s
        (s2, I r2) = k s
    in (s1 `mappend` s2, I $ r1 * r2)

  div_ (Eval j) (Eval k) = Eval $ \s ->
    let (s1, I r1) = j s
        (s2, I r2) = k s
    in (s1 `mappend` s2, I $ div r1 r2)

  eq (Eval j) (Eval k) = Eval $ \s ->
    let (s1, I r1) = j s
        (s2, I r2) = k s
        res = r1 == r2
    in (s1 `mappend` s2, B res)

  lte (Eval j) (Eval k) = Eval $ \s ->
    let (s1, I r1) = j s
        (s2, I r2) = k s
    in (s1 `mappend` s2, B $ r1 <= r2)

-- | statement instance
instance Stmt Eval where
  if_ (Eval c) (Eval t) (Eval e) = Eval $ \s ->
    let (s1, B rc) = c s
        (s2, rt) = t s1
        (s3, re) = e s1
    in if rc
       then (s2, rt) -- strict evaluations
       else (s3, re)

  while a b = if_ a (seq b (while a b)) skip

  var v = Eval $ \s -> (s, s M.! v) --an unhandled exception if var not in map

  let_ v (Eval x) = Eval $ \s ->
    let (s1, r) = x s
    in (M.insert v r s1, NoOp)

  seq (Eval a) (Eval b) = Eval $ \s ->
    let (s1, r1) = a s
        (s2, r2) = b s1
    in (s2, r2)

  skip = Eval $ \s -> (s, NoOp)

--------------------------------------------------------------------------------
-- Extension in Data Constructor, adding Strings to the language
--------------------------------------------------------------------------------
data NewPrims = NI Int | NB Bool | S String | Skip -- addition in constructors
  deriving (Eq, Show, Ord)

-- | an eval monad, without all the sugar
newtype NewEval a = NewEval {runNewEval :: VarStore NewPrims ->
                                        (VarStore NewPrims, NewPrims)}

nEmptyState :: VarStore NewPrims
nEmptyState = M.singleton "" Skip

instance StrExpr NewEval where  -- additional operator to handle strings
  slit str = NewEval $ \s -> (s, S str)

--- The rest is the same
-- | Bool instances
instance BoolExpr NewEval where
  tru = NewEval $ \s -> (s, NB True)
  fls = NewEval $ \s -> (s, NB False)
  bEq (NewEval j) (NewEval j') = NewEval $ \s ->
    let (s1, r1) = j s
        (s2, r2) = j' s
    in (s1 `mappend` s2, NB $ r1 == r2)
  bnot (NewEval j) = NewEval $ \x ->
    let (s1, NB r) = j x
    in (s1, NB $ not r)

-- | Arithmetic instances
instance ArExpr NewEval where
  lit i = NewEval $ \s -> (s, NI i)
  neg (NewEval j) = NewEval $ \s ->
    let (s', NI r) = j s
    in (s', NI $ negate r)

  add (NewEval j) (NewEval k) = NewEval $ \s ->
    let (s1, NI r1) = j s
        (s2, NI r2) = k s
    in (s1 `mappend` s2, NI $ r1 + r2)

  sub x y = add x $ neg y

  mul (NewEval j) (NewEval k) = NewEval $ \s ->
    let (s1, NI r1) = j s
        (s2, NI r2) = k s
    in (s1 `mappend` s2, NI $ r1 * r2)

  div_ (NewEval j) (NewEval k) = NewEval $ \s ->
    let (s1, NI r1) = j s
        (s2, NI r2) = k s
    in (s1 `mappend` s2, NI $ div r1 r2)

  eq (NewEval j) (NewEval k) = NewEval $ \s ->
    let (s1, NI r1) = j s
        (s2, NI r2) = k s
        res = r1 == r2
    in (s1 `mappend` s2, NB res)

  lte (NewEval j) (NewEval k) = NewEval $ \s ->
    let (s1, NI r1) = j s
        (s2, NI r2) = k s
    in (s1 `mappend` s2, NB $ r1 <= r2)

-- | statement instance
instance Stmt NewEval where
  if_ (NewEval c) (NewEval t) (NewEval e) = NewEval $ \s ->
    let (s1, NB rc) = c s
        (s2, rt) = t s1
        (s3, re) = e s1
    in if rc
       then (s2, rt) -- strict evaluations
       else (s3, re)

  while a b = if_ a (seq b (while a b)) skip

  var v = NewEval $ \s -> (s, s M.! v) --an unhandled exception if var not in map

  let_ v (NewEval x) = NewEval $ \s ->
    let (s1, r) = x s
    in (M.insert v r s1, Skip)

  seq (NewEval a) (NewEval b) = NewEval $ \s ->
    let (s1, r1) = a s
        (s2, r2) = b s1
    in (s2, r2)

  skip = NewEval $ \s -> (s, Skip)

-- | Compatible Extension for Lists

instance Lists NewEval where
  llit (NewEval x) = NewEval $ \s ->
    let (s1, r) = x s
    in (s1, [r])



--------------------------------------------------------------------------------  
-- Testing
--------------------------------------------------------------------------------

-- we can run this like so: runEval ifTest emptyState
ifTest :: Eval Int
ifTest =  if_ (bEq tru fls) (add (lit 1) (lit 2)) (add (lit 1) (lit 1))

-- run like: runEval varTest (M.insert "x" (I 100) emptyState
varTest :: Eval Int
varTest = var "x"

stringTest :: NewEval String
stringTest = slit "what"

letStringTest :: NewEval String
letStringTest = seq (let_ "x" (slit "thisisx")) stringTest

letTest :: Eval Int
letTest = seq (let_ "x" (add (lit 2) (lit 3))) $
          seq (let_ "x" (add (var "x") (lit 1))) varTest

whileTest :: Eval Int
-- this program is actually a lisp, see:
whileTest = seq
            (let_ "x" (lit 0))
            (while (lte (var "x") (lit 10))
             (let_ "x" (add (var "x") (lit 1))))

seqTest :: Eval Int
seqTest = seq (let_ "x" (lit 100)) (add (var "x") (var "x"))
