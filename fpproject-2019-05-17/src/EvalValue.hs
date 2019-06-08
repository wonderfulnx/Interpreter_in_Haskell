-- | 这是其中一种实现方式的代码框架。你可以参考它，或用你自己的方式实现，只要按需求完成 evalValue :: Program -> Result 就行。
module EvalValue where

import AST
import Control.Monad.State

data Value
  = VBool Bool
  | VInt Int
  | VChar Char
  -- ... more
  deriving (Show, Eq)

instance Ord Value where
  VInt v1 <= VInt v2 = (v1 <= v2)
  VChar v1 <= VChar v2 = (v1 <= v2)

data Context = Context { -- 可以用某种方式定义上下文，用于记录变量绑定状态
                          } deriving (Show, Eq)

type ContextState a = StateT Context Maybe a

------------------------------ get excepted value function --------------------------

getBool :: Expr -> ContextState Bool
getBool e = do
  ev <- eval e
  case ev of
    VBool b -> return b
    _ -> lift Nothing

getInt :: Expr -> ContextState Int
getInt e = do
  ev <- eval e
  case ev of
    VInt i -> return i
    _ -> lift Nothing

binaryBool :: Expr -> Expr -> (Bool -> Bool -> Bool) -> ContextState Value
binaryBool e1 e2 func = do
  ev1 <- getBool e1
  ev2 <- getBool e2
  return (VBool $ func ev1 ev2)

binaryInt :: Expr -> Expr -> (Int -> Int -> Int) -> ContextState Value
binaryInt e1 e2 func = do
  ev1 <- getInt e1
  ev2 <- getInt e2
  return (VInt $ func ev1 ev2)

valueCompare :: Expr -> Expr -> (Value -> Value -> Bool) -> ContextState Value
valueCompare e1 e2 func = do
  ev1 <- eval e1
  ev2 <- eval e2
  if (func ev1 ev2) then return $ VBool True else return $ VBool False

---------------------------------- Evaluate Funtion ---------------------------------

eval :: Expr -> ContextState Value

-------------- Bool section -----------
eval (EBoolLit b) = return $ VBool b
eval (ENot e) = getBool e >>= \b -> return (VBool $ not b)
eval (EAnd e1 e2) = binaryBool e1 e2 (&&)
eval (EOr e1 e2) = binaryBool e1 e2 (||)

-------------- Int section -----------
eval (EIntLit i) = return $ VInt i
eval (EAdd e1 e2) = binaryInt e1 e2 (+)
eval (ESub e1 e2) = binaryInt e1 e2 (-)
eval (EMul e1 e2) = binaryInt e1 e2 (*)
eval (EDiv e1 e2) = binaryInt e1 e2 div
eval (EMod e1 e2) = binaryInt e1 e2 mod

-------------- Expr section -----------
eval (ECharLit c) = return $ VChar c
eval (EEq e1 e2) = valueCompare e1 e2 (==)
eval (ENeq e1 e2) = valueCompare e1 e2 (/=)
eval (ELt e1 e2) = valueCompare e1 e2 (<)
eval (EGt e1 e2) = valueCompare e1 e2 (>)
eval (ELe e1 e2) = valueCompare e1 e2 (<=)
eval (EGe e1 e2) = valueCompare e1 e2 (>=)
eval (EIf e1 e2 e3) = do
  con <- getBool e1
  if con then eval e2 else eval e3

eval _ = undefined



---------------------------- some my own test --------------------------------
eq_false = EEq (EBoolLit True) (EBoolLit False)
lt_true = ELt (EIntLit 5) (EIntLit 6)
lt_wrong_1 = ELt (EBoolLit True) (EBoolLit False)
lt_wrong_2 = ELt (EIntLit 7) (ECharLit 'm')

evalValueExpr :: Expr -> Maybe (Value, Context)
evalValueExpr exp = runStateT (eval exp) $ Context {  }

-----------------------------------------------------------------------------

evalProgram :: Program -> Maybe Value
evalProgram (Program adts body) = evalStateT (eval body) $
  Context {  } -- 可以用某种方式定义上下文，用于记录变量绑定状态

evalValue :: Program -> Result
evalValue p = case evalProgram p of
  Just (VBool b) -> RBool b
  Just (VInt i) -> RInt i
  Just (VChar c) -> RChar c
  _ -> RInvalid
