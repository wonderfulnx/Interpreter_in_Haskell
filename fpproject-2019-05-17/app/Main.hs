module Main where

import AST
import EvalValue
import EvalType

pro1 = Program [] $ EEq (EIntLit 42) (EIntLit 42)

pro2 = Program [] $ EEq (EIntLit 42) (ECharLit 'A')

main :: IO ()
main = undefined
