{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import System.Timeout(timeout)

import AST
import EvalValue
import EvalType

-- Usage: stack runghc test/myTest.hs


addTimeout test = timeout (3*10^6) test >>= assertJustVerbose "3 seconds timeout exceeded"
testsWithTimeouts = wrap addTimeout htf_thisModulesTests -- magical preprocessing! 2019-03-06
main = htfMain testsWithTimeouts

-- my_expr_let_0 = (let x = 3 in x `mod` 2)
my_expr_let_0 = ELet ("x",(EIntLit 3)) (EMod (EVar "x") (EIntLit 2))
-- my_expr_let_0 + x （x是上一个式子中的变量，这里应该类型未知）
my_expr_let_bad_0 = EAdd my_expr_let_0 (EVar "x")

-- my_expr_fact = if x == 0 then 1 else x * fact (x - 1)
my_expr_fact = EIf (EEq (EVar "x") (EIntLit 0))
  (EIntLit 1)
  (EMul 
    (EVar "x") 
    (EApply 
      (EVar "fact") 
      (ESub (EVar "x") (EIntLit 1))
      )
    )

-- my_expr_letrec = (let fact = my_expr_fact in fact 5)
my_expr_letrec_0 = ELetRec "fact" ("x", TInt) (my_expr_fact, TInt) (EApply (EVar "fact") (EIntLit 5))
-- x是前方的变量
my_expr_letrec_bad_0 = EAdd my_expr_letrec_0 (EVar "x")
my_expr_letrec_bad_1 = ELetRec "fact" ("x", TInt) (my_expr_fact, TInt) (EApply (EVar "fact") (EVar "x"))

test_let_right_1 = assertEqual (Just TInt) (EvalType.evalType $ Program [] my_expr_let_0)
test_let_wrong_1 = assertEqual (Nothing) (EvalType.evalType $ Program [] my_expr_let_bad_0)
test_letrec_right_1 = assertEqual (Just TInt) (EvalType.evalType $ Program [] my_expr_letrec_0)
test_letrec_wrong_1 = assertEqual (Nothing) (EvalType.evalType $ Program [] my_expr_letrec_bad_0)
test_letrec_wrong_2 = assertEqual (Nothing) (EvalType.evalType $ Program [] my_expr_letrec_bad_1)
