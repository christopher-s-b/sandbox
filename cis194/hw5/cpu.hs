{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Parser
import StackVM

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a


-- exercise 5
instance Expr Program where
  lit (IVal n) = [PushI n]
  lit (BVal n) = [PushB n]
  add l r = l ++ r ++ [StackVM.Add]
  mul l r = l ++ r ++ [StackVM.Mul]


compile :: String -> Maybe Program
compile = parseExp lit add mul

-- testExp :: Expr a => Maybe a
-- testExp = parseExp lit add mul "(3 * -4) + 5"

-- testProgram = testExp :: Maybe Program
