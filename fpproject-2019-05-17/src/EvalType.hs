-- | 这是其中一种实现方式的代码框架。你可以参考它，或用你自己的方式实现，只要按需求完成 evalType :: Program -> Maybe Type 就行。
module EvalType where

import AST
import Control.Monad.State
-- import qualified Data.Map as Map
import qualified Data.Map.Strict as Map

-- store name and type pair in binds
data Context = Context { binds :: Map.Map String Type }
  deriving (Show, Eq) -- 可以用某种方式定义上下文，用于记录变量绑定状态

type ContextState a = StateT Context Maybe a

isBool :: Expr -> ContextState Type
isBool e = do
  et <- eval e
  case et of
    TBool -> return TBool
    _ -> lift Nothing

isInt :: Expr -> ContextState Type
isInt e = do
  et <- eval e
  case et of
    TInt -> return TInt
    _ -> lift Nothing

isChar :: Expr -> ContextState Type
isChar e = do
  et <- eval e
  case et of
    TChar -> return TChar
    _ -> lift Nothing

-- judge if type if e1 equals to e2, return ContextState t if true
typeEq :: Expr -> Expr -> ContextState Type
typeEq e1 e2 = do
  et1 <- eval e1
  et2 <- eval e2
  if et1 == et2 then return et1 else lift Nothing

-- judge if type if e1 equals to e2, return ContextState t if true
typeComp :: Expr -> Expr -> ContextState Type
typeComp e1 e2 = do
  et1 <- eval e1
  et2 <- eval e2
  if et1 == et2 && (et1 /= TBool)
    then return et1
    else lift Nothing

add_bind :: String -> Type -> Context -> Context
add_bind vn vt Context { binds = ms } =
  Context { binds = Map.insert vn vt ms}

find_bind :: String -> Context -> ContextState Type
find_bind vn Context { binds = ms } =
  case Map.lookup vn ms of
    Just tr -> return tr
    _ -> lift Nothing

with_var :: String -> Type -> ContextState Type -> ContextState Type
with_var vn vt a = do
  c0 <- get
  put $ add_bind vn vt c0
  t1 <- a
  put c0
  return t1
  
eval :: Expr -> ContextState Type
eval (EBoolLit _) = return TBool
eval (ENot e) = isBool e
eval (EAnd e1 e2) = isBool e1 >> isBool e2
eval (EOr e1 e2) = isBool e1 >> isBool e2

eval (EIntLit _) = return TInt
eval (EAdd e1 e2) = isInt e1 >> isInt e2
eval (ESub e1 e2) = isInt e1 >> isInt e2
eval (EMul e1 e2) = isInt e1 >> isInt e2
eval (EDiv e1 e2) = isInt e1 >> isInt e2
eval (EMod e1 e2) = isInt e1 >> isInt e2

eval (ECharLit _) = return TChar
eval (EEq e1 e2) = typeEq e1 e2 >> return TBool
eval (ENeq e1 e2) = typeEq e1 e2 >> return TBool
eval (ELt e1 e2) = typeComp e1 e2 >> return TBool
eval (EGt e1 e2) = typeComp e1 e2 >> return TBool
eval (ELe e1 e2) = typeComp e1 e2 >> return TBool
eval (EGe e1 e2) = typeComp e1 e2 >> return TBool
eval (EIf e1 e2 e3) = isBool e1 >> typeEq e2 e3

-- WRONG! And with var not done yet, in Let and LetRec they all need put the env back
-- eval (ELambda (pn, pt) e) = do
--   c <- get
--   put $ add_bind pn pt c
--   rt <- eval e
--   return $ TArrow pt rt

eval (ELambda (pn, pt) e) = do
  rt <- with_var pn pt $ eval e
  return $ TArrow pt rt

eval (ELet (str, e1) e2) = do
  t1 <- eval e1
  tr <- with_var str t1 $ eval e2
  -- c <- get
  -- put $ add_bind str t1 c
  -- t2 <- eval e2
  return tr

eval (ELetRec f (x, tx) (e1, ty) e2) = do
  c0 <- get
  put $ add_bind f (TArrow tx ty) (add_bind x tx c0)
  t1 <- eval e1
  tr <- eval e2
  if t1 == ty then
    return tr
  else lift Nothing

eval (EVar s) = do 
  c <- get
  find_bind s c

eval (EApply e1 e2) = do
  (TArrow tx tr) <- eval e1
  t2 <- eval e2
  if tx == t2 then
    return tr
  else lift Nothing

-- ECase

eval _ = undefined

---------------------------- some my own test --------------------------------

-- expr_apply = EVar "even"
expr_let = ELet ("x",(EIntLit 3)) (EMod (EVar "x") (EIntLit 2))
expr_my_bad = EAdd expr_let (EVar "x")

evalTypeExpr :: Expr -> Maybe (Type, Context)
evalTypeExpr exp = runStateT (eval exp) $ Context { binds = Map.empty }

default_context :: Context
default_context = Context {binds = Map.fromList []}

-----------------------------------------------------------------------------

evalType :: Program -> Maybe Type
evalType (Program adts body) = evalStateT (eval body) $
  Context { binds = Map.fromList [] } -- 可以用某种方式定义上下文，用于记录变量绑定状态
