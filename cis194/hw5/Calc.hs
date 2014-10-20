module Calc where

import ExprT
import Parser


-- exercise 1
eval :: ExprT -> Integer
eval e = case e of
  Lit n -> n
  Add l r -> (eval l) + (eval r)
  Mul l r -> (eval l) * (eval r)

-- eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20


-- exercise 2
evalStr :: String -> Maybe Integer
evalStr s = fmap eval $ parseExp Lit Add Mul s

-- parseExp Lit Add Mul "(2+3)*4"
-- parseExp Lit Add Mul "2+3*"


-- exercise 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit n = Lit n
  add l r = Add l r
  mul l r = Mul l r

-- mul (add (lit 2) (lit 3)) (lit 4) :: ExprT == Mul (Add (Lit 2) (Lit 3)) (Lit 4)

reify :: ExprT -> ExprT
reify = id

-- reify $ mul (add (lit 2) (lit 3)) (lit 4)


-- exercise 4
instance Expr Integer where
  lit n = n
  add = (+)
  mul = (*)

instance Expr Bool where
  lit n = n > 0
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Show, Eq)
newtype Mod7 = Mod7 Integer deriving (Show, Eq)

instance Expr MinMax where
  lit n = MinMax n
  add (MinMax l) (MinMax r) = MinMax $ if l > r then l else r
  mul (MinMax l) (MinMax r) = MinMax $ if l > r then r else l

instance Expr Mod7 where
  lit n = Mod7 (n `mod` 7)
  add (Mod7 l) (Mod7 r) = Mod7 $ (l + r) `mod` 7
  mul (Mod7 l) (Mod7 r) = Mod7 $ (l * r) `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7
