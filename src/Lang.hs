{-# LANGUAGE TypeSynonymInstances #-}

module Lang where

import Control.Monad.State
import qualified Data.Map as M

-- | Begin the typeClasses!

class BoolExpr b where
  tru  :: b Bool
  fls  :: b Bool
  bEq  :: b Bool -> b Bool -> b Bool
  bnot :: b Bool -> b Bool

class ArExpr a where
  lit  :: Int -> a Int
  neg  :: a Int -> a Int
  add  :: a Int -> a Int -> a Int
  sub  :: a Int -> a Int -> a Int
  mul  :: a Int -> a Int -> a Int
  div_ :: a Int -> a Int -> a Int
  eq   :: a Int -> a Int -> a Bool
  lte  :: a Int -> a Int -> a Bool

class Control r where
  var   :: String -> r a
  let_  :: String -> r a -> r b
  if_   :: r Bool -> r a -> r a -> r a
  while :: r Bool -> r a -> r a
  seq   :: r a -> r b -> r b 
  skip  :: r a

-- | Primitive values that this language can use
data Prims = I Int | B Bool | S String | NoOp
  deriving (Eq, Show, Ord)

-- | Map to hold let bound variables
type VarStore a = M.Map String a

-- | an eval monad, without all the sugar
newtype Eval a = Eval {runEval :: VarStore Prims -> (VarStore Prims, Prims)}

emptyState :: a -> VarStore a
emptyState = M.singleton ""

instance BoolExpr Eval where
  tru = Eval $ \s -> (s, B True)
  fls = Eval $ \s -> (s, B False)
  bEq (Eval j) (Eval j') = Eval $ \s ->
    let (s1, r1) = j s
        (s2, r2) = j' s
    in (s1 `mappend` s2, B $ r1 == r2)
  bnot (Eval j) = Eval $ \x ->
    let (s1, (B r)) = j x
    in (s1, B $ not r)

-- instance BoolExpr MonadTest where
--   tru             = return $ True
--   fls             = return $ False
--   bEq x y = do
--     x' <- x
--     y' <- y
--     return (x == y)
--   bnot x = x >>= return . not

-- instance ArExpr Prims where
--   lit               = I
--   neg  (I i)        = I $ negate i
--   add  (I i) (I i') = I $ i + i'
--   mul  (I i) (I i') = I $ i * i'
--   sub  (I i) (I i') = I $ i - i'
--   div_ (I i) (I i') = I $ div i i'
--   eq   (I i) (I i') = B $ i == i'
--   lte  (I i) (I i') = B $ i <= i'

instance ArExpr Eval where
  lit i = Eval $ \s -> (s, I i)
  neg (Eval j) = Eval $ \s ->
    let (s', I r) = j s
    in (s', I $ negate r)
  add (Eval j) (Eval k) = Eval $ \s ->
    let (s1, I r1) = j s
        (s2, I r2) = k s
    in (s1 `mappend` s2, I $ r1 + r2)
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

instance ArExpr Maybe where
  lit = return
  neg (Just x) = return $ negate x
  add (Just x) (Just y) = return $ x + y
  eq (Just x) (Just y) = return $ x == y

-- instance Control Prims where
--   if_ (B True)  t e = t
--   if_ (B False) t e = e
--   while (B True)  e = e
--   while (B False) e = skip
--   skip              = NoOp
--   -- let_

-- test1 :: (ArExpr r) => r Int
-- test1 = sub (mul (lit 5) (lit 10)) (div_ (lit 2) (lit 2))
