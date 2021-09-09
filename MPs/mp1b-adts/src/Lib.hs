--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where


--- Problems
--- ========

--- Algebraic Data Types
--- --------------------

data List a = Cons a (List a)
            | Nil
  deriving (Show, Eq)

data Exp = IntExp Integer
         | PlusExp [Exp]
         | MultExp [Exp]
  deriving (Show, Eq)

--- ### list2cons

-- don't forget to put the type declaration or you will lose points!
list2cons :: [a] -> List a
list2cons [] = Nil
list2cons (x:xs) = Cons x (list2cons xs)
--- ### cons2list

-- don't forget to put the type declaration or you will lose points!
cons2list :: List a -> [a]
cons2list Nil = []
cons2list (Cons a (xs)) = a:(cons2list xs)

--- ### eval

-- don't forget to put the type declaration or you will lose points!
eval :: Exp -> Integer
eval (IntExp x) = x
eval (PlusExp []) = 0
eval (MultExp []) = 1
--eval 

--- ### list2cons'

-- don't forget to put the type declaration or you will lose points!
list2cons' :: [a] -> List a
list2cons' l = foldr (Cons) Nil l

--- ### BinTree

-- data Node :: a -> BinTree a -> BinTree a -> BinTree a
-- data Leaf :: BinTree a


data BinTree a = Leaf 
               | Node a (BinTree a) (BinTree a) deriving(Show)

--- ### sumTree

-- don't forget to put the type declaration or you will lose points!
sumTree :: Num a => BinTree a -> a
sumTree (Leaf) = 0
sumTree (Node a l r) = a + (sumTree l) + (sumTree r)

--- ### SimpVal

data SimpVal = IntVal (Integer)
               | BoolVal (Bool)
               | StrVal (String)
               | ExnVal (String)
               deriving (Show)


--- ### liftIntOp

-- don't forget to put the type declaration or you will lose points!
liftIntOp :: (Integer -> Integer -> Integer) -> SimpVal -> SimpVal -> SimpVal
liftIntOp f (IntVal i1) (IntVal i2) = IntVal (f i1 i2)
liftIntOp f _           _           = ExnVal "not an IntVal!"
