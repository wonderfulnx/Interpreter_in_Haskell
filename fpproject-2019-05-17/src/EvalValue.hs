-- | 这是其中一种实现方式的代码框架。你可以参考它，或用你自己的方式实现，只要按需求完成 evalValue :: Program -> Result 就行。
module EvalValue where

import AST
import Control.Monad.State
import qualified Data.Map.Strict as Map

--------------------------- Value definition ------------------------
data Value
  = VBool Bool
  | VInt Int
  | VChar Char
  | VLambda String Expr
  -- | VLambda String Expr Value
  -- | Nil
  -- ... more
  deriving (Show, Eq)

instance Ord Value where
  VInt v1 <= VInt v2 = (v1 <= v2)
  VChar v1 <= VChar v2 = (v1 <= v2)

---------------------------- Context definition -----------------------
data Context = Context { binds :: Map.Map String Value }
  deriving (Show, Eq)

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

-------------------------------- Context Edition Function ---------------------------

add_bind :: String -> Value -> Context -> Context
add_bind vn ve Context { binds = ms } =
  Context { binds = Map.insert vn ve ms}

find_bind :: String -> Context -> ContextState Value
find_bind vn Context { binds = ms } =
  case Map.lookup vn ms of
    Just val -> return val
    _ -> lift Nothing

withVar :: String -> Value -> ContextState Value -> ContextState Value
withVar vn ve a = do
  c0 <- get
  put $ add_bind vn ve c0
  val1 <- a
  put c0
  return val1

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

-------------- Complex section -----------
eval (ELambda (pn, pt) e) = return $ VLambda pn e

eval (ELet (str, e1) e2) = do
  v1 <- eval e1
  er <- withVar str v1 $ eval e2
  return er

eval (ELetRec f (x, tx) (e1, ty) e2) = do
  c0 <- get
  put $ add_bind f (VLambda x e1) c0
  er <- eval e2
  put c0
  return er

eval (EVar s) = do
  c <- get
  find_bind s c

eval (EApply e1 e2) = do
  (VLambda vn ve) <- eval e1
  v2 <- eval e2
  er <- withVar vn v2 $ eval ve
  return er

-- ECase

eval _ = undefined


---------------------------- some my own test --------------------------------

-- True == False
eq_false = EEq (EBoolLit True) (EBoolLit False)
-- 5 < 6
lt_true = ELt (EIntLit 5) (EIntLit 6)
-- True < False
lt_wrong_1 = ELt (EBoolLit True) (EBoolLit False)
-- 7 < 'm'
lt_wrong_2 = ELt (EIntLit 7) (ECharLit 'm')

-- \x -> x + 1
my_lambda_expr = ELambda ("x", TInt) (EAdd (EVar "x") (EIntLit 1))
-- (\x -> x + 1) 2
my_apply_expr = EApply my_lambda_expr (EIntLit 2)

-- (\x -> \x -> x + 1) 3 4   // in haskell should be 5
my_lambda_expr_2 = ELambda ("x", TInt) my_lambda_expr
my_apply_expr_2 = EApply (EApply my_lambda_expr_2 (EIntLit 3)) (EIntLit 100)

-- my_let_expr_0 = (let x = 3 in x `mod` 2)
my_let_expr_0 = ELet ("x",(EIntLit 3)) (EMod (EVar "x") (EIntLit 2))
-- my_let_expr_0 + x （x是上一个式子中的变量，这里应该类型未知）
my_let_expr_bad_0 = EAdd my_let_expr_0 (EVar "x")

-- let solution = \a -> \b -> a + b in ...
aplusb_expr = ELetRec "solution" ("a",TInt) (ELambda ("b",TInt) (EAdd (EVar "a") (EVar "b")),TArrow TInt TInt) (EApply (EApply (EVar "solution") (EIntLit 1)) (EIntLit 2))

complex_tail = (ELet ("a",EIntLit 1) (ELet ("b",EIntLit 2) (ELet ("c",EIntLit 3) (ELet ("d",EApply (EApply (EVar "solution") (EVar "b")) (EVar "c")) (EApply (EApply (EVar "solution") (EVar "a")) (EVar "d"))))))

evalValueExpr :: Expr -> Maybe (Value, Context)
evalValueExpr exp = runStateT (eval exp) $ Context { binds = Map.fromList [] }

two_lambda_expr = ELambda ("y", TInt) (ELambda ("x", TInt) (EAdd (EVar "x") (EVar "y")))

-- 很极端情况：(\x -> \x -> x + x) 1 2

-----------------------------------------------------------------------------

evalProgram :: Program -> Maybe Value
evalProgram (Program adts body) = evalStateT (eval body) $
  Context { binds = Map.fromList [] } -- 可以用某种方式定义上下文，用于记录变量绑定状态

evalValue :: Program -> Result
evalValue p = case evalProgram p of
  Just (VBool b) -> RBool b
  Just (VInt i) -> RInt i
  Just (VChar c) -> RChar c
  _ -> RInvalid
