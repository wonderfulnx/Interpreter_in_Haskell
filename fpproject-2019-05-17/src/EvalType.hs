-- | 这是其中一种实现方式的代码框架。你可以参考它，或用你自己的方式实现，只要按需求完成 evalType :: Program -> Maybe Type 就行。
module EvalType where

import AST
import Control.Monad.State
import qualified Data.Map.Strict as Map

---------------------- store name and type pair in binds ----------------------
data Context = Context { binds :: Map.Map String Type }
  deriving (Show, Eq) -- 可以用某种方式定义上下文，用于记录变量绑定状态

type ContextState a = StateT Context Maybe a

------------------------------ judge is type function -------------------------

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

-- only int and char can order
isOrd :: Type -> Bool
isOrd t = case t of
  TInt -> True
  TChar -> True
  _ -> False

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
  if et1 == et2 && (isOrd et1)
    then return et1
    else lift Nothing

---------------------------- Context Edition Function ---------------------------

add_bind :: String -> Type -> Context -> Context
add_bind vn vt Context { binds = ms } =
  Context { binds = Map.insert vn vt ms}

find_bind :: String -> Context -> ContextState Type
find_bind vn Context { binds = ms } =
  case Map.lookup vn ms of
    Just tr -> return tr
    _ -> lift Nothing

withVar :: String -> Type -> ContextState Type -> ContextState Type
withVar vn vt a = do
  c0 <- get
  put $ add_bind vn vt c0
  t1 <- a
  put c0
  return t1

-- 添加一个adt的一个构造函数
add_cons_fun :: String -> Map.Map String Type -> (String, [Type]) -> Map.Map String Type
add_cons_fun datan ms (fun, tys) =
  let fun_t = foldr (\x -> \y -> TArrow x y) (TData datan) tys in
  Map.insert fun fun_t ms

-- 添加一个ADT的所有构造函数
add_adt_funs :: Map.Map String Type -> ADT -> Map.Map String Type
add_adt_funs ms (ADT name fs) = 
  foldl (add_cons_fun name) ms fs

-- consume a input pattern type
consume_type :: Type -> Pattern -> ContextState Type
consume_type t0 pat = do
  case t0 of
    (TArrow t1 t2) -> do
      evalPattern pat t1
      return t2
    _ -> lift Nothing

---------------------------------- Evaluate Pattern ---------------------------------

-- 计算一个pattern是否满足Type，返回结果
evalPattern :: Pattern -> Type -> ContextState Type
evalPattern (PBoolLit _) TBool = return TBool
evalPattern (PIntLit _) TInt = return TInt
evalPattern (PCharLit _) TChar = return TChar
evalPattern (PVar s) ty = do
  c0 <- get
  put $ add_bind s ty c0
  return ty
evalPattern (PData fun pats) ty = do
  c0 <- get
  tfun <- find_bind fun c0
  tr <- evalP tfun pats
  if (tr == ty) then return tr
  else lift Nothing
  where 
    evalP tp [] = return tp
    evalP tp (p:ps) = do
      ts <- consume_type tp p
      evalP ts ps

evalPattern _ _ = lift Nothing

---------------------------------- Evaluate Expr ---------------------------------

eval :: Expr -> ContextState Type
-------------- Bool section -----------
eval (EBoolLit _) = return TBool
eval (ENot e) = isBool e
eval (EAnd e1 e2) = isBool e1 >> isBool e2
eval (EOr e1 e2) = isBool e1 >> isBool e2

-------------- Int section -----------
eval (EIntLit _) = return TInt
eval (EAdd e1 e2) = isInt e1 >> isInt e2
eval (ESub e1 e2) = isInt e1 >> isInt e2
eval (EMul e1 e2) = isInt e1 >> isInt e2
eval (EDiv e1 e2) = isInt e1 >> isInt e2
eval (EMod e1 e2) = isInt e1 >> isInt e2

-------------- Expr section -----------
eval (ECharLit _) = return TChar
eval (EEq e1 e2) = typeEq e1 e2 >> return TBool
eval (ENeq e1 e2) = typeEq e1 e2 >> return TBool
eval (ELt e1 e2) = typeComp e1 e2 >> return TBool
eval (EGt e1 e2) = typeComp e1 e2 >> return TBool
eval (ELe e1 e2) = typeComp e1 e2 >> return TBool
eval (EGe e1 e2) = typeComp e1 e2 >> return TBool
eval (EIf e1 e2 e3) = isBool e1 >> typeEq e2 e3

-------------- Complex section -----------
eval (ELambda (pn, pt) e) = do
  rt <- withVar pn pt $ eval e
  return $ TArrow pt rt

eval (ELet (str, e1) e2) = do
  t1 <- eval e1
  tr <- withVar str t1 $ eval e2
  return tr

eval (ELetRec f (x, tx) (e1, ty) e2) = do
  c0 <- get
  put $ add_bind f (TArrow tx ty) c0
  t1 <- withVar x tx $ eval e1
  tr <- eval e2
  put c0
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

eval (ECase e0 []) = lift Nothing
eval (ECase e0 ((pat, exp):cas)) = do
  t0 <- eval e0
  c0 <- get
  evalPattern pat t0
  t1 <- eval exp
  put c0
  evalPE t0 t1 cas
  where
    evalPE pt et [] = return et
    evalPE pt et ((p, e):cs) = do
      c0 <- get
      evalPattern p pt
      expt <- eval e
      if (et == expt) then evalPE pt et cs
      else lift Nothing

---------------------------- some my own test --------------------------------

expr_my_fact = EIf (EEq (EVar "x") (EIntLit 0))
  (EIntLit 1)
  (EMul 
    (EVar "x") 
    (EApply 
      (EVar "fact") 
      (ESub (EVar "x") (EIntLit 1))
      )
    )
expr_my_let_rec = ELetRec "fact" ("x", TInt) (expr_my_fact, TInt) (EApply (EVar "fact") (EIntLit 6))
expr_my_let_rec_bad = EAdd (expr_my_let_rec) (EVar "x")

expr_my_let = ELet ("x",(EIntLit 3)) (EMod (EVar "x") (EIntLit 2))
expr_my_let_bad = EAdd expr_my_let (EVar "x")

my_adts = [ADT "[int]" [("[]@int",[]),("::@int",[TInt,TData "[int]"])], ADT "[char]" [("[]@char",[]),("::@char",[TChar,TData "[char]"])]]

my_context :: Context
my_context = Context { binds = foldl add_adt_funs Map.empty my_adts }

evalTypeExpr :: Expr -> Maybe (Type, Context)
evalTypeExpr exp = runStateT (eval exp) $ my_context

evalTypeWith :: Expr -> Context -> Maybe Type
evalTypeWith exp ctx = evalStateT (eval exp) ctx

-----------------------------------------------------------------------------

evalType :: Program -> Maybe Type
evalType (Program adts body) = evalStateT (eval body) $
  -- 添加adt的构造函数到binds中
  Context { binds = foldl add_adt_funs Map.empty adts }

-- 可以用某种方式定义上下文，用于记录变量绑定状态
