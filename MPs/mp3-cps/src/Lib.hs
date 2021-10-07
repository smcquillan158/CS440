--- Given Code
--- ==========

module Lib where

import System.IO (hPutStrLn, hPutStr, stdout, hFlush)

import Data.List (intercalate)

import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)

--- The Types
--- ---------

data Stmt = Decl String [String] Exp
            deriving (Eq)

instance Show Stmt where
    show (Decl f params exp) = f ++ " " ++ intercalate " " params ++ " = " ++ (show exp)

data Exp = IntExp Integer
         | VarExp String
         | LamExp String Exp
         | IfExp Exp Exp Exp
         | OpExp String Exp Exp
         | AppExp Exp Exp
         deriving (Eq)

instance Show Exp where
    show (VarExp s)       = s
    show (IntExp i)       = show i
    show (LamExp x e)     = "(\\" ++ x ++ " -> " ++ (show e) ++ ")"
    show (IfExp e1 e2 e3) = "(if " ++ show e1 ++ " then " ++ show e2
                            ++ " else " ++ show e3 ++ ")"
    show (OpExp op e1 e2) = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
    show (AppExp f e)     = show f ++ " " ++ show e

ctorShow :: Exp -> String
ctorShow (VarExp s)       = "VarExp " ++ show s
ctorShow (IntExp i)       = "IntExp " ++ show i
ctorShow (LamExp x e)     = "LamExp " ++ show x ++ " (" ++ ctorShow e ++ ")"
ctorShow (IfExp e1 e2 e3) = "IfExp (" ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ") ("
                                ++ ctorShow e3 ++ ")"
ctorShow (OpExp op e1 e2) = "OpExp " ++ show op ++ " ("
                                ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ")"
ctorShow (AppExp f e)     = "AppExp (" ++ ctorShow f ++ ") (" ++ ctorShow e ++ ")"

--- Problems
--- ========

--- Manual Translation
--- ------------------

--- ### `factk :: Integer -> (Integer -> t) -> t`

factk :: Integer -> (Integer -> t) -> t
factk 0 f = f 1
factk n f = factk (n-1) (\x -> f (n*x))

--- ### `evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t`

evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t
evenoddk [] k _ = k 0
evenoddk [x] ke ko | even x = evenoddk [] (\v -> ke (v+x)) (\v -> ke (v+x))
                   | otherwise = evenoddk [] (\v -> ko (v+x)) (\v -> ko (v+x))
evenoddk (x:xs) ke ko | even x = evenoddk xs (\v -> ke  (v + x)) ko 
                      | otherwise = evenoddk xs ke (\v -> ko (v +x))  
                
--- Automated Translation
--- ---------------------

gensym :: Integer -> (String, Integer)
gensym i = ("v" ++ show i, i + 1)

--- ### Define `isSimple`

isSimple :: Exp -> Bool
isSimple exp = case exp of
    (AppExp e e1) -> False
    (IntExp a) -> True
    (VarExp a) -> True
    (IfExp e e1 e2) -> isSimple e && isSimple e1 && isSimple e2
    (OpExp o e1 e2) -> isSimple e1 && isSimple e2

--- ### Define `cpsExp` - Overview

cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)
cpsExp (IntExp n) k i = (AppExp k (IntExp n), i)
cpsExp (IfExp e1 e2 e3) k num
    | isSimple e1 = (IfExp e1 (e4) (e5), num)
    | otherwise = cpsExp e1 (LamExp v (IfExp (VarExp v) e4 e5)) n2
    where
        (v, n2) = gensym num
        (e4, _) = cpsExp e2 k n2
        (e5, _) = cpsExp e3 k n2
--- #### Define `cpsExp` for Integer and Variable Expressions

--- #### Define `cpsExp` for Application Expressions

--- #### Define `cpsExp` for Operator Expressions

--- #### Define `cpsExp` for If Expressions

--- ### Define `cpsDecl`

cpsDecl :: Stmt -> Stmt
cpsDecl = undefined
