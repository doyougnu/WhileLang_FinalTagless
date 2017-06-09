module CoreLang where

import Prelude hiding (seq)

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
-- Experiments
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

class IntLists l where
  ilit  :: Int -> l [Int]
  icons :: l Int -> l [Int] -> l [Int]
  ihead :: l [Int] -> l Int
  itail :: l [Int] -> l [Int]

-- | Another way to add lists
-- Another limitation, cannot create lists as sequenced if statements because we
-- need to be specific about the smart constructors, we have no dispatch for the
-- right "lit" call on "e"

-- class (Stmt l) => Lists2 l where
--   lllit :: a -> l a
--   lllit e = if_ tru (lit e) skip

--------------------------------------------------------------------------------
-- Stretch Goal
--------------------------------------------------------------------------------
class (Stmt t) => Tag t where
  tag :: t a -> t (Int, a)
