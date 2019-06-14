module Main where
import AST
import Parser
import Text.Megaparsec
import Data.List(intercalate)
import System.IO
import System.Environment
import System.Console.Repline
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.IO.Class
import qualified Data.Map.Strict as Map
import qualified EvalType as ET
import qualified EvalValue as EV

----------------------------- Context -------------------------------
data Context = 
  Context {
    bindsT :: Map.Map String Type, 
    bindsV :: Map.Map String EV.Value
  }
  deriving (Show, Eq)

add_bind :: String -> Type -> EV.Value -> Context -> Context
add_bind s t v Context { bindsT = t0, bindsV = v0} = 
  Context { bindsT = Map.insert s t t0, bindsV = Map.insert s v v0 }

type_ctx :: Context -> ET.Context
type_ctx Context { bindsT = t0, bindsV = v0} = 
  ET.Context { ET.binds = t0 }

value_ctx :: Context -> EV.Context
value_ctx Context { bindsT = t0, bindsV = v0} = 
  EV.Context { EV.binds = v0 }

----------------------------------------------------------------------

type StateRepl a = HaskelineT (StateT Context IO) a
type Repl a = HaskelineT IO a

cmdParse :: String -> Repl ()
cmdParse s = liftIO $ parseTest naiveHsParser s

cmdEval :: String -> StateRepl ()
cmdEval input = do
  ctx <- get
  case parse stmtNaiveHsParser "" input of
    Left bundle -> liftIO $ putStr (errorBundlePretty bundle)
    Right (SExpr body) -> do
      case ET.evalTypeWith body $ type_ctx ctx of
        Nothing -> liftIO $ putStrLn "Type Check Failed!"
        Just _ -> liftIO $ print $ getResult $ EV.evalValueWith body $ value_ctx ctx
    Right (SAssi str body) -> do
      case ET.evalTypeWith body $ type_ctx ctx of
        Nothing -> liftIO $ putStrLn "Type Check Failed!"
        Just tr -> do
          case EV.evalValueWith body $ value_ctx ctx of
            Nothing -> liftIO $ print RInvalid
            Just val -> put $ add_bind str tr val ctx


-- init message of repl
iniParse :: Repl ()
iniParse = liftIO $ putStrLn "Welcome to NaiveHS Parser!"
iniEval :: StateRepl ()
iniEval = liftIO $ putStrLn "Welcome to NaiveHs!"

quit :: [String] -> Repl ()
quit _ = abort
quitState :: [String] -> StateRepl ()
quitState _ = abort

getType :: [String] -> StateRepl ()
getType [] = liftIO $ putStrLn "No argument input!"
getType xs = do
  case parse naiveHsParser "" $ intercalate " " xs of
    Left bundle -> liftIO $ putStr (errorBundlePretty bundle)
    Right body -> do
      ctx <- get
      case ET.evalTypeWith body $ type_ctx ctx of
        Nothing -> liftIO $ putStrLn "Type Check Failed!"
        Just tr -> liftIO $ print tr

options :: [(String, [String] -> Repl ())]
options = [ ("quit", quit) ]
optionsState :: [(String, [String] -> StateRepl ())]
optionsState = [ ("quit", quitState), ("type", getType) ]

completer :: Monad m => WordCompleter m
completer n = return []
  
replParse :: IO ()
replParse = evalRepl (pure "*Parse> ") cmdParse options (Just ':') (Word completer) iniParse
replEval :: IO ()
replEval = evalStateT 
  (evalRepl (pure "*Eval> ") cmdEval optionsState (Just ':') (Word completer) iniEval)
  $ Context { bindsT = Map.empty, bindsV = Map.empty }

getResult :: Maybe EV.Value -> Result
getResult val = case val of
  Just (EV.VBool b) -> RBool b
  Just (EV.VInt i) -> RInt i
  Just (EV.VChar c) -> RChar c
  _ -> RInvalid

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
