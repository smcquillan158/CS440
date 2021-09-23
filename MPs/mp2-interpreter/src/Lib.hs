module Lib where
import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)


--- Data Types
--- ----------

--- ### Environments and Results

type Env  = H.HashMap String Val
type PEnv = H.HashMap String Stmt

type Result = (String, PEnv, Env)

--- ### Values

data Val = IntVal Int
         | BoolVal Bool
         | CloVal [String] Exp Env
         | ExnVal String
    deriving (Eq)

instance Show Val where
    show (IntVal i) = show i
    show (BoolVal i) = show i
    show (CloVal xs body env) = "<" ++ show xs   ++ ", "
                                    ++ show body ++ ", "
                                    ++ show env  ++ ">"
    show (ExnVal s) = "exn: " ++ s

--- ### Expressions

data Exp = IntExp Int
         | BoolExp Bool
         | FunExp [String] Exp
         | LetExp [(String,Exp)] Exp
         | AppExp Exp [Exp]
         | IfExp Exp Exp Exp
         | IntOpExp String Exp Exp
         | BoolOpExp String Exp Exp
         | CompOpExp String Exp Exp
         | VarExp String
    deriving (Show, Eq)

--- ### Statements

data Stmt = SetStmt String Exp
          | PrintStmt Exp
          | QuitStmt
          | IfStmt Exp Stmt Stmt
          | ProcedureStmt String [String] Stmt
          | CallStmt String [Exp]
          | SeqStmt [Stmt]
    deriving (Show, Eq)

--- Primitive Functions
--- -------------------

intOps :: H.HashMap String (Int -> Int -> Int)
intOps = H.fromList [ ("+", (+))
                    , ("-", (-))
                    , ("*", (*))
                    , ("/", (div))
                    ]

boolOps :: H.HashMap String (Bool -> Bool -> Bool)
boolOps = H.fromList [ ("and", (&&))
                     , ("or", (||))
                     ]

compOps :: H.HashMap String (Int -> Int -> Bool)
compOps = H.fromList [ ("<", (<))
                     , (">", (>))
                     , ("<=", (<=))
                     , (">=", (>=))
                     , ("/=", (/=))
                     , ("==", (==))
                     ]

--- Problems
--- ========

--- Lifting Functions
--- -----------------

liftIntOp :: (Int -> Int -> Int) -> Val -> Val -> Val
liftIntOp (div) (IntVal x) (IntVal 0) = ExnVal "Division by 0"
liftIntOp op (IntVal x) (IntVal y) = IntVal $ op x y 
liftIntOp _ _ _ = ExnVal "Cannot lift"

liftBoolOp :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftBoolOp op (BoolVal x) (BoolVal y) = BoolVal (op x y)
liftBoolOp _ _ _ = ExnVal "Cannot lift"

liftCompOp :: (Int -> Int -> Bool) -> Val -> Val -> Val
liftCompOp op (IntVal x) (IntVal y) = BoolVal (op x y)
liftCompOp _ _ _ = ExnVal "Cannot lift" 

--- Eval
--- ----

eval :: Exp -> Env -> Val

--- ### Constants

eval (IntExp i)  _ = (IntVal i)
eval (BoolExp i) _ = (BoolVal i)

--- ### Variables

eval (VarExp s) env = case H.lookup s env of
                        Just val -> val
                        Nothing -> ExnVal "No match in env"


--- ### Arithmetic

eval (IntOpExp op e1 e2) env = case H.lookup op intOps of  
                                Just rat -> liftIntOp rat (eval e1 env) (eval e2 env)
                                Nothing -> ExnVal "exn"

--- ### Boolean and Comparison Operators

eval (BoolOpExp op e1 e2) env = case H.lookup op boolOps of
    Just rat -> liftBoolOp rat (eval e1 env) (eval e2 env)
    Nothing -> ExnVal "exn"

eval (CompOpExp op e1 e2) env = case H.lookup op compOps of
    Just rat -> liftCompOp rat (eval e1 env) (eval e2 env)
    Nothing -> ExnVal "exn"

--- ### If Expressions

eval (IfExp e1 e2 e3) env = case eval e1 env of
    BoolVal i -> if i then (eval e2 env) else (eval e3 env)
    _ -> ExnVal "Condition is not a Bool"

--- ### Functions and Function Application

eval (FunExp params body) env = CloVal params body env

eval (AppExp e1 args) env = case eval e1 env of
    CloVal params fbody envt -> eval fbody (union envt (H.fromList (zip params (evalArgs args env))))
   
-- 
--     CloVal xs body env1 -> eval body (H.fromList ((H.toList env1) ++ (zip args xs)))  
                                     

--- ### Let Expressions

eval (LetExp pairs body) env = undefined

evalArgs :: [Exp] -> Env -> [Val]    
evalArgs args env = fmap (`eval` env) args

--- Statements
--- ----------

-- Statement Execution
-- -------------------

exec :: Stmt -> PEnv -> Env -> Result
exec (PrintStmt e) penv env = (val, penv, env)
    where val = show $ eval e env

--- ### Set Statements

exec (SetStmt var e) penv env = undefined

--- ### Sequencing

exec (SeqStmt []) penv env = undefined

--- ### If Statements

exec (IfStmt e1 s1 s2) penv env = undefined

--- ### Procedure and Call Statements

exec p@(ProcedureStmt name args body) penv env = undefined

exec (CallStmt name args) penv env = undefined
