module Main where
import AST
import Parser
import Text.Megaparsec
import System.Console.Repline
import Control.Monad
import Control.Monad.IO.Class
import System.Environment
import System.IO
import EvalType(evalType)
import EvalValue(evalValue)

type Repl a = HaskelineT IO a

cmdEval :: String -> Repl ()
cmdEval input = do
  case parse naiveHsParser "" input of
    Left bundle -> liftIO $ putStr (errorBundlePretty bundle)
    Right body -> do
      case evalType (Program [] body) of
        Nothing -> liftIO $ putStrLn "Type Check Failed!"
        Just _ -> liftIO $ print $ evalValue (Program [] body)

cmdParse :: String -> Repl ()
cmdParse s = liftIO $ parseTest naiveHsParser s

-- init message of repl
iniParse :: Repl ()
iniParse = liftIO $ putStrLn "Welcome to NaiveHS Parser!"
iniEval :: Repl ()
iniEval = liftIO $ putStrLn "Welcome to NaiveHs!"

-- repl function quit
quit :: [String] -> Repl ()
quit _ = abort

options :: [(String, [String] -> Repl ())]
options = [
    ("quit", quit)
  ]

completer :: Monad m => WordCompleter m
completer n = return []
  
replParse :: IO ()
replParse = evalRepl (pure "*Parse> ") cmdParse options (Just ':') (Word completer) iniParse

replEval :: IO ()
replEval = evalRepl (pure "*Eval> ") cmdEval options (Just ':') (Word completer) iniEval


main :: IO ()
main = do 
  args <- getArgs
  if length args /= 1 then
    help
  else if head args == "parse" then
    replParse
  else if head args == "eval" then
    replEval
  else
    help
  where
    help = do
      putStrLn "Usage: fpproject-exe COMMAND"
      putStrLn ""
      putStrLn "Available commands:"
      putStrLn "  parse         REPL of parsing the input."
      putStrLn "  eval          REPL of evaluating the input."
