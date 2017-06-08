{-# LANGUAGE FlexibleContexts #-}
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

-- | an eval monad
data Eval a = E (State (VarStore Prims) Prims)

emptyState :: VarStore Prims
emptyState = M.singleton "" NoOp

-- | Bool instances
-- instance BoolExpr Eval where
--   tru = Eval $ \s -> (s, B True)
--   fls = Eval $ \s -> (s, B False)
--   bEq (Eval j) (Eval j') = Eval $ \s ->
--     let (s1, r1) = j s
--         (s2, r2) = j' s
--     in (s1 `mappend` s2, B $ r1 == r2)
--   bnot (Eval j) = Eval $ \x ->
--     let (s1, B r) = j x
--     in (s1, B $ not r)

-- instance BoolExpr Eval where -- Cannot monadify because type synonym instances
-- cannot be partially applied. So I always need to apply Eval to an a which means
-- I cannot make it a typeclass instance. So we need to wrap it around a datatype
instance BoolExpr Eval where 
  tru = E . return $ B True
  fls = E . return $ B False
  bEq (E j) (E k) = E e
    where e = do
          (B x) <- j
          (B y) <- k
          return . B $ x == y
  bnot (E j) = E e
    where e = do
          (B b) <- j
          return . B $ b
            
-- | Arithmetic instances
arHelper f c' (E x) (E y) = do
  (I x') <- x
  (I y') <- y
  return . c' $ f x' y'

wrapper f c' x y = E e
  where e = arHelper f c' x y

instance ArExpr Eval where
  lit = E . return . I 
  neg (E i) = E e
    where e = do
           (I i') <- i
           return . I $ i'
  add x y  = wrapper (+) I x y
  div_ x y = wrapper (div) I x y
  mul x y  = wrapper (*) I x y
  eq x y   = wrapper (==) B x y
  lte x y  = wrapper (<=) B x y

-- -- | statement instance
instance Stmt Eval where
  if_ (E c) (E t) (E e) = E e 
    where e = do
            (B c') <- c
            t' <- t
            e' <- e
            return $ if c' then t' else e'

  var v = E e --Cannot use bind or get, No instance declarations, because of general type
    where e = do
            st <- get
            return $ st M.! v

  let_ v (E x) = E e
    where e = do
            st <- get
            x' <- x
            put $ M.insert v x' st
            return NoOp

  seq (E x) (E y) = E $ do x;y

  skip = E $ return NoOp

--------------------------------------------------------------------------------
-- Extension in Data Constructor, adding Strings to the language
--------------------------------------------------------------------------------
data NewPrims = NI Int | NB Bool | S String | F Float | Skip -- addition in constructors
  deriving (Eq, Show, Ord)

-- | an new eval monad
data NEval a = NE (State (VarStore NewPrims) NewPrims)

nEmptyState :: VarStore NewPrims
nEmptyState = M.singleton "" Skip

instance StrExpr NEval where  -- additional operator to handle strings
  slit = NE . return . S

--- The rest is the same
instance BoolExpr NEval where 
  tru = NE . return $ NB True
  fls = NE . return $ NB False
  bEq (NE j) (NE k) = NE e
    where e = do
          (NB x) <- j
          (NB y) <- k
          return . NB $ x == y
  bnot (NE j) = NE e
    where e = do
          (NB b) <- j
          return . NB $ b
            
-- | Arithmetic instances
narHelper f c' (NE x) (NE y) = do
  (NI x') <- x
  (NI y') <- y
  return . c' $ f x' y'

nwrapper f c' x y = NE e
  where e = narHelper f c' x y

instance ArExpr NEval where
  lit = NE . return . NI 
  neg (NE i) = NE e
    where e = do
           (NI i') <- i
           return . NI $ i'
  add x y  = nwrapper (+) NI x y
  div_ x y = nwrapper (div) NI x y
  mul x y  = nwrapper (*) NI x y
  eq x y   = nwrapper (==) NB x y
  lte x y  = nwrapper (<=) NB x y

-- -- | statement instance
instance Stmt NEval where
  if_ (NE c) (NE t) (NE e) = NE e 
    where e = do
            (NB c') <- c
            t' <- t
            e' <- e
            return $ if c' then t' else e'

  var v = NE e --Cannot use bind or get, No instance declarations, because of general type
    where e = do
            st <- get
            return $ st M.! v

  let_ v (NE x) = NE e
    where e = do
            st <- get
            x' <- x
            put $ M.insert v x' st
            return Skip

  seq (NE x) (NE y) = NE $ do x;y

  skip = NE $ return Skip

-- | Compatible Extension for Lists

-- fails cannot match NewPrims and [NewPrims]
-- instance Lists NewEval where
--   llit (NewEval x) = NewEval $ \s ->
--     let (s1, r) = x s
--     in (s1, [r])

-- instance Tag NewEval where
--   tag ()

--------------------------------------------------------------------------------  
-- Testing
--------------------------------------------------------------------------------

-- we can run this like so: runEval ifTest emptyState
ifTest :: Eval Int
ifTest =  if_ (bEq tru fls) (add (lit 1) (lit 2)) (add (lit 1) (lit 1))

-- run like: runEval varTest (M.insert "x" (I 100) emptyState
varTest :: Eval Int
varTest = var "x"

stringTest :: NEval String
stringTest = slit "what"

letStringTest :: NEval String
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

mulTest :: Eval Int
mulTest = mul (lit 3) (lit 2)

-- λ> :t seqTest
-- seqTest :: Eval Int
-- λ> :t mulTest

-- <interactive>:1:1: error:
--     No instance for (Eq (Eval Int)) arising from a use of ‘it’
