--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where

-- This line imports the Prelude module without certain functions
import Prelude hiding ( take, drop, reverse
                      , zip, zipWith
                      , map, foldl, foldr
                      , iterate, repeat
                      , replicate, cycle
                      , (++)
                      )
-- When you are allowed to use builtin functions Prepend them with "P."
-- for example `P.take`
import qualified Prelude as P

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake

-- don't forget to put the type declaration or you will lose points!
mytake :: Int -> [a] -> [a]
mytake n _ | n <= 0 = []
mytake _ [] = []
mytake n (x:xs) = x:mytake (n-1) xs


--- ### mydrop

-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a]
mydrop n xs | n <= 0 = xs
mydrop _ [] = []
mydrop n (_:xs) = mydrop (n-1) xs

--- ### rev

-- don't forget to put the type declaration or you will lose points!
rev :: [a] -> [a]
rev xs = revv xs []
    where revv [] r = r
          revv (x:xs) r  = revv xs (x:r) 

--- ### app

-- don't forget to put the type declaration or you will lose points!
app = undefined

--- ### inclist

-- don't forget to put the type declaration or you will lose points!
inclist :: Num a => [a] -> [a]
inclist [] = []
inclist (x:xs) = (x+1):(inclist xs)

--- ### sumlist

-- don't forget to put the type declaration or you will lose points!
sumlist :: Num a => [a] -> a
sumlist [] = 0 
sumlist (x:xs) = x + sumlist xs

--- ### myzip

-- don't forget to put the type declaration or you will lose points!
myzip :: [a] -> [b] -> [(a,b)]
myzip [] _ = []
myzip _ [] = []
myzip (x:xs) (y:ys) = (x,y):(myzip xs ys)

--- ### addpairs

-- don't forget to put the type declaration or you will lose points!
addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs [] _ = []
addpairs _ [] = []
addpairs (x:xs) (y:ys) = (x+y):(addpairs xs ys) 

--- ### ones

-- don't forget to put the type declaration or you will lose points!
ones :: [Integer]
ones = 1:ones

--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats :: [Integer]
nats = inc 0

inc :: Integer -> [Integer]
inc a = a:(inc (a+1))
 

--- ### fib

-- don't forget to put the type declaration or you will lose points!
fib :: [Integer]
fib = 0 : (fibs 0 1)
    where fibs x y = y:(fibs y (x + y)) 

--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add :: Ord a => a -> [a] -> [a]
add a [] = [a]
add a (x:xs) | a < x = a:x:xs
             | a == x = (x:xs)
             | otherwise = x : add a xs

--- ### union

-- don't forget to put the type declaration or you will lose points!
union :: Ord a => [a] -> [a] -> [a]
union [] x = x
union x [] = x
union xs@(x:xt) ys@(y:yt) | x < y = x : union xt ys
                          | x > y = y : union xs yt
                          | x == y = x : union xt yt
                             
--- ### intersect

-- don't forget to put the type declaration or you will lose points!
intersect :: Ord a => [a] -> [a] -> [a]
intersect [] x = x
intersect x [] = x
intersect x y =  

--- ### powerset

-- don't forget to put the type declaration or you will lose points!
powerset = undefined

--- Higher Order Functions
--- ----------------------

--- ### inclist'

-- don't forget to put the type declaration or you will lose points!
inclist' :: Num a => [a] -> [a]
inclist' l = fmap (1+) l

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
-- sumlist' :: [a] -> [a]
-- sumlist' l = fold (+) 0 l 

-- fold :: (a -> b -> b) -> b -> [a] -> b
-- fold _ v [] = v
-- fold f v (x:xs) = f x $ fold f v xs
