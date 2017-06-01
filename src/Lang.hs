module Lang where

import Control.Monad.State
import qualified Data.Map as M
import Prelude hiding (seq)

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

class Stmt r where
  var   :: String -> r a
  let_  :: String -> r a -> r b
  if_   :: r Bool -> r a -> r a -> r a
  while :: r Bool -> r a -> r a
  seq   :: r a -> r b -> r b 
  skip  :: r a

-- | Primitive values that this language can use
data Prims = I Int | B Bool | NoOp
  deriving (Eq, Show, Ord)

-- | Map to hold let bound variables
type VarStore a = M.Map String a

-- | an eval monad, without all the sugar
newtype Eval a = Eval {runEval :: VarStore Prims -> (VarStore Prims, Prims)}

emptyState :: VarStore Prims
emptyState = M.singleton "" NoOp

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

-- | Testing
-- we can run this like so: runEval ifTest (emptyState (I 0))
ifTest :: Eval Int
ifTest =  if_ (bEq tru fls) (add (lit 1) (lit 2)) (add (lit 1) (lit 1))

-- run like: runEval varTest (M.insert "x" (I 100) $ emptyState (I 0))
varTest :: Eval Int
varTest = var "x"

letTest :: Eval Int
letTest = seq (let_ "x" (add (lit 2) (lit 3))) $
          seq (let_ "x" (add (var "x") (lit 1))) varTest

whileTest :: Eval Int
whileTest = seq
            (let_ "x" (lit 0))
            (while (lte (var "x") (lit 10))
             (let_ "x" (add (var "x") (lit 1))))

seqTest :: Eval Int
seqTest = seq (let_ "x" (lit 100)) (add (var "x") (var "x"))
