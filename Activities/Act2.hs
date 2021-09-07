data Tree =
     TInt Integer
   | TOp String Tree Tree
   deriving Show

t1 = TOp "*" (TOp "+" (TInt 20) (TInt 1))
             (TOp "-" (TInt 10) (TInt 8))

tt = TOp "+" (TInt 20) (TInt 1)

swap :: Tree -> Tree
swap t@(TInt i) = t
swap t@(TOp s l r) =  TOp s (swap r) (swap l) 

calc :: Tree -> Integer
calc t@(TInt i) = i
calc t@(TOp s (lt) (rt)) = useOp s (calc lt) (calc rt)
   
useOp s l r | s == "+" = l + r
            | s == "-" = l - r
            | s == "*" = l * r
     
