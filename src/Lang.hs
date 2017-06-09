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
data Eval a = E (State (VarStore Prims) Prims) -- Extending Core Lang with State

emptyState :: VarStore Prims
emptyState = M.singleton "" NoOp

-- instance BoolExpr Eval where -- Cannot monadify because type synonym
-- instances cannot be partially applied. So I always need to apply Eval to an a
-- which means I cannot make it a typeclass instance. So we need to wrap it
-- around a datatype
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
-- Extension in Data Constructor, adding Strings, Floats to the language
--------------------------------------------------------------------------------
data NewPrims = NI Int
              | NB Bool
              | S String -- extensions in constructor dimension
              | F Float
              | L [NewPrims]
              | Skip 
  deriving (Eq, Show, Ord)

-- | a new eval monad
data NEval a = NE (State (VarStore NewPrims) NewPrims) 

nEmptyState :: VarStore NewPrims
nEmptyState = M.singleton "" Skip

runNEval (NE s) = runState s nEmptyState

-- | Compatible Extension, Exponentials
instance Exp NEval where
  exp (NE x) (NE y) = NE e
    where e = do
            (NI x') <- x
            (NI y') <- y
            return . NI $ x' ^ y'

-- | Adding non-compatible extension for strings 
instance StrExpr NEval where  -- additional operator to handle strings
  slit = NE . return . S

-- | Adding non-compatible extension for Floats
fHelper f (NE x) (NE y) = do
  (F x') <- x
  (F y') <- y
  return . F $ f x' y'

fwrapper f x y = NE e
  where e = fHelper f x y

instance Floats NEval where
  flit = NE . return . F
  fneg (NE f) = NE e
    where e = do
            (F f') <- f
            return . F $ negate f'
  fsqrt (NE f) = NE e
    where e = do
            (F f') <- f
            return . F $ sqrt f'
  fadd  x y = fwrapper (+)  x y
  fmul  x y = fwrapper (*)  x y

-- The rest is the same
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

-- | statement instance
instance Stmt NEval where
  if_ (NE c) (NE t) (NE e) = NE e 
    where e = do
            (NB c') <- c
            t' <- t
            e' <- e
            return $ if c' then t' else e'

  var v = NE e 
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
instance IntLists NEval where
  ilit = NE . return . L . (:[]) . NI
  icons (NE x) (NE xs) = NE e
    where e = do
            (NI x') <- x
            (L xs') <- xs
            return . L $ ((NI x') : xs')
  ihead (NE xs) = NE e
    where e = do
            (L xs') <- xs
            return $ head xs'
  itail (NE xs) = NE e
    where e = do
            (L xs') <- xs
            return . L $ tail xs'

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

listTest :: NEval [Int]
listTest = ilit 3


-- λ> :t seqTest
-- seqTest :: Eval Int
-- λ> :t mulTest

-- <interactive>:1:1: error:
--     No instance for (Eq (Eval Int)) arising from a use of ‘it’
