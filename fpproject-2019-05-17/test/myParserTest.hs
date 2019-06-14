-- This is a Parser Test file
--    Usage: 
--        stack build
--        stack runghc test/myParserTest.hs

import Test.Tasty
import Test.Tasty.HUnit
import AST
import Parser
import Text.Megaparsec

data Example = Example { 
  name :: String,
  expr :: String,
  tree :: Expr
} deriving (Show)

mkTestCase :: Example -> [TestTree]
mkTestCase (Example n exp tree) =
  [testCase n $ parseMaybe myParser exp @?= Just tree]

mkTestSuite :: String -> [Example] -> TestTree
mkTestSuite n es = testGroup n $ es >>= mkTestCase

testTasks :: [TestTree]
testTasks =
  [ mkTestSuite "simple"  simple,
    mkTestSuite "complex" complex
  ]

main = defaultMain $ testGroup "" testTasks

-- simple cases
simple = 
  [
    Example {
      name = "l00_EBoolLit",
      expr = "true",
      tree = EBoolLit True
    },
    Example {
      name = "l01_EIntLit",
      expr = "-42",
      tree = EIntLit (-42)
    },
    Example {
      name = "l02_ECharLit",
      expr = "\'@\'",
      tree = ECharLit '@'
    },
    Example {
      name = "l03_ENot",
      expr = "!false",
      tree = ENot (EBoolLit False)
    },
    Example {
      name = "l03_ENot_bad",
      expr = "!(-42)",
      tree = ENot (EIntLit (-42))
    },
    Example {
      name = "l04_EAnd",
      expr = "true && false",
      tree = EAnd (EBoolLit True) (EBoolLit False)
    },
    Example {
      name = "l04_EAnd_bad",
      expr = "true && (-42)",
      tree = EAnd (EBoolLit True) (EIntLit (-42))
    },
    Example {
      name = "l05_EOr",
      expr = "false || true",
      tree = EOr (EBoolLit False) (EBoolLit True)
    },
    Example {
      name = "l05_EOr_bad",
      expr = "false || (-42)",
      tree = EOr (EBoolLit False) (EIntLit (-42))
    },
    Example {
      name = "l06_EAdd",
      expr = "40 + 2",
      tree = EAdd (EIntLit 40) (EIntLit 2)
    },
    Example {
      name = "l06_EAdd_bad",
      expr = "40 + false",
      tree = EAdd (EIntLit 40) (EBoolLit False)
    },
    Example {
      name = "l07_ESub",
      expr = "40 - 2",
      tree = ESub (EIntLit 40) (EIntLit 2)
    },
    Example {
      name = "l07_ESub_bad",
      expr = "40 - false",
      tree = ESub (EIntLit 40) (EBoolLit False)
    },
    Example {
      name = "l08_EMul",
      expr = "6 * 7",
      tree = EMul (EIntLit 6) (EIntLit 7)
    },
    Example {
      name = "l08_EMul_bad",
      expr = "40 * false",
      tree = EMul (EIntLit 40) (EBoolLit False)
    },
    Example {
      name = "l09_EDiv",
      expr = "85 / 2",
      tree = EDiv (EIntLit 85) (EIntLit 2)
    },
    Example {
      name = "l09_EDiv_bad",
      expr = "40 / false",
      tree = EDiv (EIntLit 40) (EBoolLit False)
    },
    Example {
      name = "l10_EMod",
      expr = "19 % 5",
      tree = EMod (EIntLit 19) (EIntLit 5)
    },
    Example {
      name = "l10_EMod_bad",
      expr = "40 % false",
      tree = EMod (EIntLit 40) (EBoolLit False)
    },
    Example {
      name = "l11_EEq",
      expr = "42 == 42",
      tree = EEq (EIntLit 42) (EIntLit 42)
    },
    Example {
      name = "l11_EEq_bad",
      expr = "0 == false",
      tree = EEq (EIntLit 0) (EBoolLit False)
    },
    Example {
      name = "l12_ENeq",
      expr = "\'@\' /= \'@\'",
      tree = ENeq (ECharLit '@') (ECharLit '@')
    },
    Example {
      name = "l12_ENeq_bad",
      expr = "\'0\' /= 48",
      tree = ENeq (ECharLit '0') (EIntLit 48)
    },
    Example {
      name = "l13_ELt",
      expr = "\'A\' < \'a\'",
      tree = ELt (ECharLit 'A') (ECharLit 'a')
    },
    Example {
      name = "l13_ELt_bad",
      expr = "false < true",
      tree = ELt (EBoolLit False) (EBoolLit True)
    },
    Example {
      name = "l14_EGt",
      expr = "\'a\' > \'A\'",
      tree = EGt (ECharLit 'a') (ECharLit 'A')
    },
    Example {
      name = "l14_EGt_bad",
      expr = "\'0\' > 48",
      tree = EGt (ECharLit '0') (EIntLit 48)
    },
    Example {
      name = "l15_ELe",
      expr = "\'@\' <= \'@\'",
      tree = ELe (ECharLit '@') (ECharLit '@')
    },
    Example {
      name = "l15_ELe_bad",
      expr = "\'0\' <= 48",
      tree = ELe (ECharLit '0') (EIntLit 48)
    },
    Example {
      name = "l16_EGe",
      expr = "4 >= 2",
      tree = EGe (EIntLit 4) (EIntLit 2)
    },
    Example {
      name = "l16_EGe_bad",
      expr = "\'0\' >= 48",
      tree = EGe (ECharLit '0') (EIntLit 48)
    },
    Example {
      name = "l17_EIf",
      expr = "if false then 42 else 233",
      tree = EIf (EBoolLit False) (EIntLit 42) (EIntLit 233)
    },
    Example {
      name = "l17_EIf_0_bad",
      expr = "if \'T\' then 42 else 233",
      tree = EIf (ECharLit 'T') (EIntLit 42) (EIntLit 233)
    },
    Example {
      name = "l17_EIf_1_bad",
      expr = "if true then \'0\' else 48",
      tree = EIf (EBoolLit True) (ECharLit '0') (EIntLit 48)
    },
    Example {
      name = "l18_ELambda_only_typecheck",
      expr = "\\x:(int)->x+1",
      tree = ELambda ("x",TInt) (EAdd (EVar "x") (EIntLit 1))
    },
    Example {
      name = "l18_ELambda_bad",
      expr = "\\x:(bool)->x+1",
      tree = ELambda ("x",TBool) (EAdd (EVar "x") (EIntLit 1))
    },
    Example {
      name = "l19_EApply",
      expr = "(\\x:(int)->x+1) $ 41",
      tree = EApply (ELambda ("x",TInt) (EAdd (EVar "x") (EIntLit 1))) (EIntLit 41)
    },
    Example {
      name = "l19_EApply_bad",
      expr = "(\\x:(int)->x+1) $ false",
      tree = EApply (ELambda ("x",TInt) (EAdd (EVar "x") (EIntLit 1))) (EBoolLit False)
    },
    Example {
      name = "l20_ELet_0",
      expr = "let x = 41 in x + 1",
      tree = ELet ("x",EIntLit 41) (EAdd (EVar "x") (EIntLit 1))
    },
    Example {
      name = "l20_ELet_0_bad",
      expr = "let x = false in x + 1",
      tree = ELet ("x",EBoolLit False) (EAdd (EVar "x") (EIntLit 1))
    },
    Example {
      name = "l20_ELet_1",
      expr = "let even = (\\x:(int)->x%2==0) in even $ 42",
      tree = ELet ("even",ELambda ("x",TInt) (EEq (EMod (EVar "x") (EIntLit 2)) (EIntLit 0))) (EApply (EVar "even") (EIntLit 42))
    },
    Example {
      name = "l20_ELet_1_bad",
      expr = "let even = \\x:(int)->x%2==0 in even $ \'9\'",
      tree = ELet ("even",ELambda ("x",TInt) (EEq (EMod (EVar "x") (EIntLit 2)) (EIntLit 0))) (EApply (EVar "even") (ECharLit '9'))
    },
    Example {
      name = "l21_ELetRec_0",
      expr = "def fact:(int) = (\\x:(int)->if x == 0 then 1 else x * fact $ (x - 1)) in fact $ 5",
      tree = ELetRec "fact" ("x",TInt) (EIf (EEq (EVar "x") (EIntLit 0)) (EIntLit 1) (EMul (EVar "x") (EApply (EVar "fact") (ESub (EVar "x") (EIntLit 1)))),TInt) (EApply (EVar "fact") (EIntLit 5))
    },
    Example {
      name = "l21_ELetRec_0_0_bad",
      expr = "def fact:(bool) = (\\x:(int)->if x == 0 then 1 else x * fact $ (x - 1)) in fact $ 5",
      tree = ELetRec "fact" ("x",TInt) (EIf (EEq (EVar "x") (EIntLit 0)) (EIntLit 1) (EMul (EVar "x") (EApply (EVar "fact") (ESub (EVar "x") (EIntLit 1)))),TBool) (EApply (EVar "fact") (EIntLit 5))
    },
    Example {
      name = "l21_ELetRec_0_1_bad",
      expr = "def fact:(int) = (\\x:(bool)->if x == 0 then 1 else x * fact $ (x - 1)) in fact $ 5",
      tree = ELetRec "fact" ("x",TBool) (EIf (EEq (EVar "x") (EIntLit 0)) (EIntLit 1) (EMul (EVar "x") (EApply (EVar "fact") (ESub (EVar "x") (EIntLit 1)))),TInt) (EApply (EVar "fact") (EIntLit 5))
    },
    Example {
      name = "l21_ELetRec_0_2_bad",
      expr = "def fact:(int) = (\\x:(int)->if x == 0 then 1 else x * fact $ (x - 1)) in fact $ true",
      tree = ELetRec "fact" ("x",TInt) (EIf (EEq (EVar "x") (EIntLit 0)) (EIntLit 1) (EMul (EVar "x") (EApply (EVar "fact") (ESub (EVar "x") (EIntLit 1)))),TInt) (EApply (EVar "fact") (EBoolLit True))
    }
  ]

-- complex cases
complex =
  [
    Example {
      name = "00_aplusb",
      expr = "def solution:(int->int) = (\\a:(int)->\\b:(int)->a+b) in (let a = 1 in (let b = 2 in (let c = 3 in (let d = solution $ b $ c in (solution $ a $ d)))))",
      tree = ELetRec "solution" ("a",TInt) (ELambda ("b",TInt) (EAdd (EVar "a") (EVar "b")),TArrow TInt TInt) (ELet ("a",EIntLit 1) (ELet ("b",EIntLit 2) (ELet ("c",EIntLit 3) (ELet ("d",EApply (EApply (EVar "solution") (EVar "b")) (EVar "c")) (EApply (EApply (EVar "solution") (EVar "a")) (EVar "d"))))))
    },
    Example {
      name = "01_lcm",
      expr = "def gcd:(int -> int) = (\\a:(int)->\\b:(int)->if a == b then a else if a > b then gcd $ (a - b) $ b else gcd $ (b - a) $ a) in ( def solution:(int -> int) = (\\a:(int)->\\b:(int)->let c = gcd $ a $ b in a * b / c) in solution $ 27 $ 36 )",
      tree = ELetRec "gcd" ("a",TInt) (ELambda ("b",TInt) (EIf (EEq (EVar "a") (EVar "b")) (EVar "a") (EIf (EGt (EVar "a") (EVar "b")) (EApply (EApply (EVar "gcd") (ESub (EVar "a") (EVar "b"))) (EVar "b")) (EApply (EApply (EVar "gcd") (ESub (EVar "b") (EVar "a"))) (EVar "a")))),TArrow TInt TInt) (ELetRec "solution" ("a",TInt) (ELambda ("b",TInt) (ELet ("c",EApply (EApply (EVar "gcd") (EVar "a")) (EVar "b")) (EDiv (EMul (EVar "a") (EVar "b")) (EVar "c"))),TArrow TInt TInt) (EApply (EApply (EVar "solution") (EIntLit 27)) (EIntLit 36)))
    },
    Example {
      name = "02_church0",
      expr = "let zero = (\\f:(int->int)->\\x:(int)->x) in  let succ = (\\n:((int -> int) -> (int -> int))->\\f:(int -> int)->\\x:(int)->f$(n$f$x)) in let plus = (\\a:( (int -> int) -> (int -> int))->\\b:( (int -> int) -> (int -> int))->\\f:(int -> int)->\\x:(int)->let af = (a $ f) in let bf = (b $ f) in af $ (bf $ x)) in  let mult = (\\a:( (int -> int) -> (int -> int))->\\b:( (int -> int) -> (int -> int))->\\f:(int->int)->b$(a$f))in let f = \\x:(int) -> x + 1 in  let one = succ $ zero in  let two = succ $ one in  let three = succ $ two in  let five = plus $ two $ three in  let six = mult $ two $ three in  six $ f $ 0 + five $ f $ 0",
      tree = ELet ("zero",ELambda ("f",TArrow TInt TInt) (ELambda ("x",TInt) (EVar "x"))) (ELet ("succ",ELambda ("n",TArrow (TArrow TInt TInt) (TArrow TInt TInt)) (ELambda ("f",TArrow TInt TInt) (ELambda ("x",TInt) (EApply (EVar "f") (EApply (EApply (EVar "n") (EVar "f")) (EVar "x")))))) (ELet ("plus",ELambda ("a",TArrow (TArrow TInt TInt) (TArrow TInt TInt)) (ELambda ("b",TArrow (TArrow TInt TInt) (TArrow TInt TInt)) (ELambda ("f",TArrow TInt TInt) (ELambda ("x",TInt) (ELet ("af",EApply (EVar "a") (EVar "f")) (ELet ("bf",EApply (EVar "b") (EVar "f")) (EApply (EVar "af") (EApply (EVar "bf") (EVar "x"))))))))) (ELet ("mult",ELambda ("a",TArrow (TArrow TInt TInt) (TArrow TInt TInt)) (ELambda ("b",TArrow (TArrow TInt TInt) (TArrow TInt TInt)) (ELambda ("f",TArrow TInt TInt) (EApply (EVar "b") (EApply (EVar "a") (EVar "f")))))) (ELet ("f",ELambda ("x",TInt) (EAdd (EVar "x") (EIntLit 1))) (ELet ("one",EApply (EVar "succ") (EVar "zero")) (ELet ("two",EApply (EVar "succ") (EVar "one")) (ELet ("three",EApply (EVar "succ") (EVar "two")) (ELet ("five",EApply (EApply (EVar "plus") (EVar "two")) (EVar "three")) (ELet ("six",EApply (EApply (EVar "mult") (EVar "two")) (EVar "three")) (EAdd (EApply (EApply (EVar "six") (EVar "f")) (EIntLit 0)) (EApply (EApply (EVar "five") (EVar "f")) (EIntLit 0))))))))))))
    },
    Example {
      name = "04_church2",
      expr = "let zero = (\\f:(int->int)->\\x:(int)->x) in  let succ = (\\n:((int -> int) -> (int -> int))->\\f:(int -> int)->\\x:(int)->f$(n$f$x)) in let mult = (\\a:( (int -> int) -> (int -> int))->\\b:( (int -> int) -> (int -> int))->\\f:(int->bool)->b$(a$f))in mult",
      tree = ELet ("zero",ELambda ("f",TArrow TInt TInt) (ELambda ("x",TInt) (EVar "x"))) (ELet ("succ",ELambda ("n",TArrow (TArrow TInt TInt) (TArrow TInt TInt)) (ELambda ("f",TArrow TInt TInt) (ELambda ("x",TInt) (EApply (EVar "f") (EApply (EApply (EVar "n") (EVar "f")) (EVar "x")))))) (ELet ("mult",ELambda ("a",TArrow (TArrow TInt TInt) (TArrow TInt TInt)) (ELambda ("b",TArrow (TArrow TInt TInt) (TArrow TInt TInt)) (ELambda ("f",TArrow TInt TBool) (EApply (EVar "b") (EApply (EVar "a") (EVar "f")))))) (EVar "mult")))
    },
    Example {
      name = "06_and",
      expr = "def test:(bool) = \\x:(bool) -> x || test $ false in test $ true",
      tree = ELetRec "test" ("x",TBool) (EOr (EVar "x") (EApply (EVar "test") (EBoolLit False)),TBool) (EApply (EVar "test") (EBoolLit True))
    },
    Example {
      name = "08_apply",
      expr = "def sum3: (int -> (int -> int)) = \\x1:(int)->\\x2:(int)->\\x3:(int)->x1 + x2 + x3 in sum3 $ 1 $ 1 $ 1 $ 1",
      tree = ELetRec "sum3" ("x1",TInt) (ELambda ("x2",TInt) (ELambda ("x3",TInt) (EAdd (EAdd (EVar "x1") (EVar "x2")) (EVar "x3"))),TArrow TInt (TArrow TInt TInt)) (EApply (EApply (EApply (EApply (EVar "sum3") (EIntLit 1)) (EIntLit 1)) (EIntLit 1)) (EIntLit 1))
    },
    Example {
      name = "09_apply",
      expr = "def sum3: (int -> (int -> int)) = \\x1:(int)->\\x2:(int)->\\x3:(int)->x1 + x2 + x3 in let f = sum3 $ 1 $ 1 in f $ 1",
      tree = ELetRec "sum3" ("x1",TInt) (ELambda ("x2",TInt) (ELambda ("x3",TInt) (EAdd (EAdd (EVar "x1") (EVar "x2")) (EVar "x3"))),TArrow TInt (TArrow TInt TInt)) (ELet ("f",EApply (EApply (EVar "sum3") (EIntLit 1)) (EIntLit 1)) (EApply (EVar "f") (EIntLit 1)))
    }
  ]


-- data Example = Example { 
--   na :: String,
--   expr :: String,
--   tree :: Expr
-- } deriving (Show)

-- getExample :: Example -> Example
-- getExample (Example n (Program _ exp) _ _) = Example n "" exp

-- showExample :: [Example] -> IO ()
-- showExample xs = go (map getExample xs)
--   where
--     go [] = return ()
--     go (y:ys) = do
--       print y
--       go ys
