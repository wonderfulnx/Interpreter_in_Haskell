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
  -- VLambda 表示一个Lambda表达式，String保存其参数，Mapping保存局部变量(已知)
  | VLambda String Expr (Map.Map String Value)
  -- VCons 表示一个构造函数
    -- String保存其对应的代数数据类型名
    -- [Value]保存已经给定的参数值
    -- Int保存其需要的参数个数
  | VCons String [Value] Int
  -- VData 表示一个代数数据类型S
    -- String保存其对应的代数数据类型名
    -- [Value]保存其中的值
    -- 实际为VCons的特例
  -- | VData String [Value]
  deriving (Show, Eq)

instance Ord Value where
  VInt v1 <= VInt v2 = (v1 <= v2)
  VChar v1 <= VChar v2 = (v1 <= v2)

---------------------------- Context definition -----------------------
-- binds保存变量名到变量的映射
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
  Context { binds = Map.insert vn ve ms }

add_binds :: Map.Map String Value -> Context -> Context
add_binds local Context { binds = ms } = 
  Context { binds = Map.union local ms }

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

withVars :: Map.Map String Value -> ContextState Value -> ContextState Value
withVars ms a = do
  c0 <- get
  put $ add_binds ms c0
  val1 <- a
  put c0
  return val1

-- 添加一个adt的一个构造函数
add_cons_fun :: String -> Map.Map String Value -> (String, [Type]) -> Map.Map String Value
add_cons_fun datan ms (fun, tys) = 
  let fun_v = VCons fun [] $ length tys in
    Map.insert fun fun_v ms

-- 添加一个ADT的所有构造函数
add_adt_funs :: Map.Map String Value -> ADT -> Map.Map String Value
add_adt_funs ms (ADT name fs) = 
  foldl (add_cons_fun name) ms fs

---------------------------------- Evaluate Pattern ---------------------------------

-- patternEq :: Value -> Value -> Bool
-- patternEq v1 v2 = case v1 of
--   (VInt _) -> v1 == v2
--   (VChar _) -> v1 == v2
--   (VBool _) -> v1 == v2
--   (VLambda _) -> v1 == v2
--   (VCons s1 l1 v1) -> case v2 of
--     (VCons s2 l2 v2) -> s1 == s2 && v1 == v2
--     _ -> False
--   (VData s1 l1) -> case v2 of
--     (VData s2 l2) -> s1 == s2
--     _ -> False

-- 计算一个pattern，由外界给定值，返回是否匹配
evalPattern :: Pattern -> Value -> ContextState Bool
evalPattern (PBoolLit b) (VBool b0) = 
  if (b == b0) then return True
  else return False
evalPattern (PIntLit i) (VInt i0) = 
  if (i == i0) then return True
  else return False
evalPattern (PCharLit c) (VChar c0) = 
  if (c == c0) then return True
  else return False
evalPattern (PVar s) val = do
  c0 <- get
  put $ add_bind s val c0
  return True
evalPattern (PData fun pats) (VCons in_datan in_vals 0) = do
  c0 <- get
  (VCons datan [] paran) <- find_bind fun c0
  if (paran /= length pats || in_datan /= datan) then lift Nothing
  else evalP pats $ reverse in_vals
  where
    -- evalP 接受in_vals和pats具体进行匹配
    evalP [] [] = return True
    evalP (p:ps) (v:vs) = do
      br <- evalPattern p v
      if br then evalP ps vs
      else return False

evalPattern _ _ = lift Nothing

---------------------------------- Evaluate Expr ------------------------------------

eval :: Expr -> ContextState Value
-------------- Bool section -----------
eval (EBoolLit b) = return $ VBool b
eval (ENot e) = getBool e >>= \b -> return (VBool $ not b)

-- short-circuit evaluation for `and` and `or`
eval (EAnd e1 e2) = do
  ev1 <- getBool e1
  if ev1 then do
    ev2 <- getBool e2
    return $ VBool ev2
  else return $ VBool False
eval (EOr e1 e2) = do
  ev1 <- getBool e1
  if ev1 then return $ VBool True
  else do
    ev2 <- getBool e2
    return $ VBool ev2

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
eval (ELambda (pn, pt) e) = return $ VLambda pn e $ Map.fromList []

eval (ELet (str, e1) e2) = do
  v1 <- eval e1
  er <- withVar str v1 $ eval e2
  return er

eval (ELetRec f (x, tx) (e1, ty) e2) = do
  c0 <- get
  put $ add_bind f (VLambda x e1 $ Map.fromList []) c0
  er <- eval e2
  put c0
  return er

eval (EVar s) = do
  c <- get
  find_bind s c

eval (EApply e1 e2) = do
  v2 <- eval e2
  (VLambda pn exp lo) <- eval e1
  er <- withVars (Map.insert pn v2 lo) $ eval exp
  case er of
    (VLambda npn nexp nlo) -> return (VLambda npn nexp $ Map.union nlo (Map.insert pn v2 lo))
    x -> return x

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

-- aplus1_expr = let f = \x -> x + 1 in f 2
aplus1_expr = ELetRec "f" ("a",TInt) ((EAdd (EVar "a") (EIntLit 1)), TArrow TInt TInt) (EApply (EVar "f") (EIntLit 2))

-- let solution = \a -> \b -> a + b in ...
aplusb_expr = ELetRec "solution" ("a",TInt) (ELambda ("b",TInt) (EAdd (EVar "a") (EVar "b")),TArrow TInt TInt) (EApply (EApply (EVar "solution") (EIntLit 1)) (EIntLit 2))

complex_tail = (ELet ("a",EIntLit 1) (ELet ("b",EIntLit 2) (ELet ("c",EIntLit 3) (ELet ("d",EApply (EApply (EVar "solution") (EVar "b")) (EVar "c")) (EApply (EApply (EVar "solution") (EVar "a")) (EVar "d"))))))

my_adts = [ADT "[int]" [("[]@int",[]),("::@int",[TInt,TData "[int]"])], ADT "[char]" [("[]@char",[]),("::@char",[TChar,TData "[char]"])]]

evalValueExpr :: Expr -> Maybe (Value, Context)
evalValueExpr exp = runStateT (eval exp) $ Context { binds = foldl add_adt_funs Map.empty my_adts }

-- my_two_lambda_expr = (\x -> \y -> x + y) 3 4
two_lambda_expr = ELambda ("x", TInt) (ELambda ("y", TInt) (EAdd (EVar "x") (EVar "y")))
my_two_lambda_expr = EApply (EApply two_lambda_expr (EIntLit 3)) (EIntLit 4)
-- 很极端情况：(\x -> \x -> x + x) 1 2
two_lambda_expr_2 = ELambda ("x", TInt) (ELambda ("x", TInt) (EAdd (EVar "x") (EVar "x")))
my_two_lambda_expr_2 = EApply (EApply two_lambda_expr_2 (EIntLit 3)) (EIntLit 4)

-- 中间类型的lambda
my_two_lambda_expr_3 = EApply two_lambda_expr (EIntLit 3)

-- 一个复杂测例
three_lambda_expr_part1 = 
  ELambda ("y", TInt) (
    ELambda ("z", TInt) (
      EAdd (EVar "x") (EAdd (EVar "y") (EVar "z"))
    )
  )
three_lambda_expr =
  EApply (
    EApply (
      ELambda ("x", TInt) (
        EApply three_lambda_expr_part1 (EIntLit 2)
      )
    ) (EIntLit 1)) (EIntLit 3)


-----------------------------------------------------------------------------

evalProgram :: Program -> Maybe Value
evalProgram (Program adts body) = evalStateT (eval body) $
  Context { binds = foldl add_adt_funs Map.empty adts } -- 可以用某种方式定义上下文，用于记录变量绑定状态

evalValue :: Program -> Result
evalValue p = case evalProgram p of
  Just (VBool b) -> RBool b
  Just (VInt i) -> RInt i
  Just (VChar c) -> RChar c
  _ -> RInvalid
