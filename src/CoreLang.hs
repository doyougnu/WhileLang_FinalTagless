{-# LANGUAGE FlexibleInstances #-}
module CoreLang where

import Control.Monad.State
import Control.Monad.State.Class

import qualified Data.Map as M
import Prelude hiding (seq)

--------------------------------------------------------------------------------
-- Core Language
--------------------------------------------------------------------------------

-- | Boolean Expressions
class BoolExpr b where
  tru  :: b Bool
  fls  :: b Bool
  bEq  :: b Bool -> b Bool -> b Bool
  bnot :: b Bool -> b Bool

-- | Arithmetic Expressions
class ArExpr a where
  lit  :: Int -> a Int
  neg  :: a Int -> a Int
  add  :: a Int -> a Int -> a Int
  sub  :: a Int -> a Int -> a Int
  sub x y = add x $ neg y
  -- mul  :: (Eq (a Int), Num (a Int)) => a Int -> a Int -> a Int
  -- mul x 0 = x
  -- mul x y = add x (add x (sub y 1))
  mul  :: a Int -> a Int -> a Int
  div_ :: a Int -> a Int -> a Int
  eq   :: a Int -> a Int -> a Bool
  lte  :: a Int -> a Int -> a Bool

-- | Statements
class Stmt r where
  var   :: String -> r a
  let_  :: String -> r a -> r b
  if_   :: r Bool -> r a -> r a -> r a
  while :: r Bool -> r a -> r a
  while a b = if_ a (seq b (while a b)) skip
  seq   :: r a -> r b -> r b
  skip  :: r a

--------------------------------------------------------------------------------
-- Core Language Instances
--------------------------------------------------------------------------------
-- | Primitive values that this language can use
data Prims = I Int | B Bool | NoOp
  deriving (Eq, Show, Ord)

-- | Map to hold let bound variables
type VarStore a = M.Map String a

-- | an eval monad
data Eval a = E (State (VarStore Prims) Prims) -- Extending Core Lang with State

emptyState :: VarStore Prims
emptyState = M.empty

runEval (E s) = runState s emptyState

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
  if_ (E c) (E t) (E e) = E res
    where res = do
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
