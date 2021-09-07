merge :: [Integer] -> [Integer] -> [Integer]
merge []         ys                   = ys
merge xs         []                   = xs
merge xs@(x:xt) ys@(y:yt) | x <= y    = x : merge xt ys
                          | otherwise = y : merge xs yt


mergesort' :: [Integer] -> [Integer]                          
mergesort' [] = []
mergesort' [x] = [x]
mergesort' l = merge (mergesort' (take ((length l) `div` 2) l)) (mergesort' (drop ((length l) `div` 2) l))

removeNegatives :: [Integer] -> [Integer]
removeNegatives [] = []
removeNegatives (x:xs) | x < 0 = removeNegatives xs
                       | otherwise = x:removeNegatives xs
 
sumList' :: Num t => [t] -> t
sumList' [] = 0
sumList' (x:xs) = x + sumList' xs

euler1 :: [Integer] -> Integer
euler1 [] = 0 
euler1 (x:xs) | (x `mod` 3 == 0) || (x `mod` 5 == 0) = x + euler1 xs
              | otherwise = euler1 xs

hasNegatives :: [Integer] -> Bool
hasNegatives [] = False
hasNegatives (x:xs) | x < 0 = True
                    | otherwise = hasNegatives xs