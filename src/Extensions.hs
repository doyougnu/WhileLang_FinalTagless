{-# LANGUAGE FlexibleContexts #-}
module Extensions where

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

3) This approach is extensible in the evaluator dimensions so I can trivially
add state to the core language, even though nothing in the core lang mandates
that we have a global state. We could easily add tracing (writer monad) or error
handling (Maybe monad or either monad) by extending in this dimension.

4) However a major downside is that I cannot use bind or return operators in the
instances because I get no instance of Monad for xxx whatever by data constructor
is. This is a side-effect of being required to wrap my monad stack into a data
constructor so I can instantiate type classes with it.
-}

--------------------------------------------------------------------------------
-- Extension in Data Constructor, adding Strings, Floats, Lists to Language
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
nEmptyState = M.empty

runNEval (NE s) = runState s nEmptyState

--------------------------------------------------------------------------------
-- Core Lang Instances for New Primitives
--------------------------------------------------------------------------------
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
  add  = nwrapper (+) NI 
  div_ = nwrapper div NI 
  mul  = nwrapper (*) NI 
  eq   = nwrapper (==) NB 
  lte  = nwrapper (<=) NB

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

--------------------------------------------------------------------------------
-- Trivial Extensions
--------------------------------------------------------------------------------

-- | Compatible Extension for Exponentials
class Exp r where
  exp :: r Int -> r Int -> r Int

-- | Incompatible extension for strings
class StrExpr s where
  slit :: String -> s String

-- | Incompatible extension for floats
class Floats f where
  flit  :: Float -> f Float
  fneg  :: f Float -> f Float
  fsqrt  :: f Float -> f Float
  fadd  :: f Float -> f Float -> f Float
  fsub  :: f Float -> f Float -> f Float
  fsub x y = fadd x $ fneg y
  fmul  :: f Float -> f Float -> f Float

--------------------------------------------------------------------------------
-- Trivial Extension Instances
--------------------------------------------------------------------------------

-- | Compatible Extension, Exponentials
instance Exp NEval where
  exp (NE x) (NE y) = NE e
    where e = do
            (NI x') <- x
            (NI y') <- y
            return . NI $ x' ^ y'

-- | Adding non-compatible extension for strings
instance StrExpr NEval where
  slit = NE . return . S     -- additional operator to handle strings

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
  fadd = fwrapper (+)
  fmul = fwrapper (*)

--------------------------------------------------------------------------------
-- Experimental Extensions, Lists and Printing
--------------------------------------------------------------------------------
-- | For lists we must specify the input type, or else we cannot dispatch on the
-- right constructor to construct a new prims term, see failed experiment section
class IntLists l a where
  ilit  :: a -> l [a]
  icons :: l a -> l [a] -> l [a]
  ihead :: l [a] -> l a
  itail :: l [a] -> l [a]

-- | Adding a print statement to the language
-- This works because the output type is independent of input type
class (Stmt o) => Output o where
  prnt :: o a -> IO () 

--------------------------------------------------------------------------------
-- Experimental Extensions, Lists and Printing Instances
--------------------------------------------------------------------------------
-- | Compatible Extension for Lists
-- Would be the same for any other primitive value, just using Int as an example
instance IntLists NEval Int where
  ilit = NE . return . L . (:[]) . NI
  icons (NE x) (NE xs) = NE e
    where e = do
            (NI x') <- x
            (L xs') <- xs
            return . L $ (NI x' : xs')
  ihead (NE xs) = NE e
    where e = do
            (L xs') <- xs
            return $ head xs'
  itail (NE xs) = NE e
    where e = do
            (L xs') <- xs
            return . L $ tail xs'

-- | Adding printing to the language
-- Can't unbox to a generic so we have to print tags
-- unBox :: NewPrims -> [a] 
-- unBox (F  f) = [f]
-- unBox (NI x) = [x]
-- unBox (NB b) = [b]
-- unBox (S  s) = [s]
-- unBox (L xs) = concatMap unBox xs

-- | Compatible extension for printing, this works because we collapse all things
-- to a single type, IO ()
instance Output NEval where
  prnt x = print . fst $ runNEval x

--------------------------------------------------------------------------------
-- Failed Experiments
--------------------------------------------------------------------------------
-- incompatible extension for functions
-- Fails because no way to bind variables during the apply step
-- class (Stmt f) => Funs f where
--   defnUnary :: String -> String -> f a -> f a -> f b
--   defnUnary fname arg = let_ fname (let_ arg body)

--   appUnary  :: String -> f a -> f b
--   appUnary fname

-- | adding lists, should be a Compatible extension, but isn't
-- Fails because we have no polymorphism in our language so we need to be able
-- to dispatch to the right value constructor when we make the list. Could avoid
-- by directly adding Lists for each primitive
-- class Lists l where
--   llit :: l a -> l [a]
--   cons :: l a -> l [a] -> l [a]
--   nth  :: l Int -> l [a] -> l a
--   head :: l [a] -> l a
--   tail :: l [a] -> l [a]
--   -- another downside no handrolled loops
--   map_ :: (a -> b) -> l [a] -> l [b]
--   rightFold :: (a -> b -> b) -> l b -> l [a] -> l b

-- | Another way to add lists

-- Another limitation, cannot create lists as sequenced if statements because we
-- need to be specific about the smart constructors, and we have no dispatch for
-- the right "lit" call on "e"

-- class (Stmt l) => Lists2 l where
--   lllit :: a -> l a
--   lllit e = if_ tru (lit e) skip


--------------------------------------------------------------------------------
-- Stretch Goal
--------------------------------------------------------------------------------
-- this looks like it will work, but we have no way to dispatch on different
-- terms in our language. So for instance, we have no way to un-tag our terms, or
-- in other words we cannot say if you get an if, recur into the then branch,
-- return the count and then recur into the else branch. We cannot do that
-- because we do not have Pattern Matching, and because we don't have an "eval"
-- function. So this would need to be lifted to the evaluater dimension, just like
-- state

class (Stmt t) => Tag t where
  tag :: t a -> t [(Int, a)]
  
class (Stmt t) => Goto t where
  goto :: t Int -> t a -> t a

--------------------------------------------------------------------------------
-- Testing
--------------------------------------------------------------------------------

-- we can run this like so: runEval ifTest emptyState
ifTest :: (ArExpr e, BoolExpr e, Stmt e) => e Int
-- ifTest :: Eval Int
ifTest =  if_ (bEq tru fls) (add (lit 1) (lit 2)) (add (lit 1) (lit 1))

-- run like: runEval varTest (M.insert "x" (I 100) emptyState
varTest :: (Stmt e) => e Int
varTest = var "x"

stringTest :: (StrExpr s) => s String
stringTest = slit "what"

letStringTest :: NEval String
letStringTest = seq (let_ "x" (slit "thisisx")) stringTest

letTest :: (Stmt s, ArExpr s) => s Int
letTest = seq (let_ "x" (add (lit 2) (lit 3))) $
          seq (let_ "x" (add (var "x") (lit 1))) varTest

whileTest :: (Stmt s, BoolExpr s, ArExpr s) => s Int
-- this program is actually a lisp, see:
whileTest = seq
            (let_ "x" (lit 0))
            (while (lte (var "x") (lit 10))
             (let_ "x" (add (var "x") (lit 1))))

seqTest :: (Stmt s, ArExpr s) => s Int
seqTest = seq (let_ "x" (lit 100)) (add (var "x") (var "x"))

printTest :: IO ()
printTest = prnt (add (lit 3) (lit 3) :: NEval Int)

mulTest :: (ArExpr s) => s Int
mulTest = mul (lit 3) (lit 2)

listTest :: (ArExpr l, IntLists l Int) => l Int
listTest = ihead . icons (lit 3) $ ilit 3

-- I need to compose a IO monad with my state monad for these type to workout
-- fizzBuzz :: (ArExpr f, Output f, Stmt f) => IO Int
-- fizzBuzz = seq
--            (let_ "x" (lit 0))
--            (while (lte (var "x") (lit 100))
--             (seq
--              (if_ (eq
--                   (lit 0)
--                   (div_ (var "x") (lit 3)))
--              (prnt (slit "Fizz" :: NEval String))
--             (if_ (eq
--                  (lit 0)
--                  (div_ (var "x") (lit 5)))
--             (prnt (slit "Buzz" :: NEval String))
--             (prnt (var "x" :: NEval Int))))
--              (let_ "x" (add (var "x") (lit 1)))))
