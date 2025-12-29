-- module Eval(Env,eval) where
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE NamedFieldPuns #-}

module Eval(eval,searchVar,emptyEnv) where
import Types
import Span
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Word (Word64)
import Control.Monad (foldM)

eval_keep_env :: Env -> AST -> Result EvalError (Value,Env)
eval_keep_env env ast = do
    (val, _newEnv) <- eval env ast
    Ok (val, env)

eval :: Env -> AST -> Result EvalError (Value,Env)
eval env (ASTNum _ n) = Ok (ValNum n, env)
eval env (ASTVoid _) = Ok (ValVoid, env)
eval env (ASTString _ s) = Ok (ValString s, env)
eval env (ASTSymbol sp name) = do
    v <- searchVar env sp name
    Ok (v, env)
eval env (ASTAssign _ (ASTSymbol _ name) rhs) = do
    (val, env1) <- eval env rhs
    let env2 = insertVar name val env1
    Ok (val, env2)

eval env (ASTFunc _ body argSpan argName) = do
    let func = Func { 
        captureEnv = env, 
        funcBody = body, 
        funcArgName = argName, 
        funcArgSpan = argSpan 
    }
    Ok (ValFunc func, env)

-- eval env (ASTCall sp (f:arg:[])) = do
--   (fVal, env1) <- eval env f
--   func <- expectFunc (spanOf f) fVal
--   (argVal, env2) <- eval env1 arg
--   applyFunc sp env2 func argVal

--(f *args) call f(a1)(a2)... if at any point f is not a function error
eval env (ASTCall sp (f:args)) = do
  -- evaluate the function expression first
  (fVal, env1) <- eval env f
  -- now fold left through all arguments
  foldM step (fVal, env1) args
  where
    step :: (Value, Env) -> AST -> Result EvalError (Value, Env)
    step (curVal, curEnv) argAst = do
      func <- expectFunc sp curVal
      (argVal, env2) <- eval curEnv argAst
      applyFunc sp env2 func argVal
    
    applyFunc :: Span -> Env -> Func -> Value -> Result EvalError (Value, Env)
    applyFunc sp _callEnv func argVal = do
        let env' = M.insert (funcArgName func) argVal (captureEnv func)
        (res, _) <- eval env' (funcBody func)
        Ok (res, _callEnv)   -- caller env unchanged  

eval env (ASTAdd _ lhs rhs) =
    eval_binop env lhs rhs (numOp (+))

eval env (ASTSub _ lhs rhs) =
    eval_binop env lhs rhs (numOp (-))

eval env (ASTMul _ lhs rhs) =
    eval_binop env lhs rhs (numOp (*))

eval env (ASTDiv sp lhs rhs) =
    eval_binop env lhs rhs divOp
  where
    divOp (ValNum _ ) _ (ValNum 0) _ = Err ( DivisionByZero sp)
    divOp (ValNum a) _ (ValNum b) _ = Ok (ValNum (a `div` b))
    divOp (ValNum _) _ _ spb = Err (NotANumber spb)
    divOp _ spa _ _ = Err (NotANumber spa)

eval env (ASTIf _ cond thenBr elseBr) = do
    (condVal, env1) <- eval env cond
    case condVal of
      ValNum 0 -> eval_keep_env env1 elseBr
      _        -> eval_keep_env env1 thenBr

eval env ast = Err (TODO)

eval_binop
  :: Env
  -> AST
  -> AST
  -> (Value -> Span -> Value -> Span -> Result EvalError Value)
  -> Result EvalError (Value, Env)
eval_binop env lhs rhs op = do
    (v1, env1) <- eval env  lhs
    (v2, env2) <- eval env1 rhs
    v3 <- op v1 (spanOf lhs) v2 (spanOf rhs)
    Ok (v3, env2)


numOp :: (Word64 -> Word64 -> Word64)
      -> Value  -> Span -> Value -> Span -> Result EvalError Value
numOp f (ValNum a) _ (ValNum b) _ = Ok (ValNum (f a b))
numOp _ (ValNum a) _ _ spb = Err (NotANumber spb)
numOp _ _ spa _ _ = Err (NotANumber spa)
--todo handle errors


expectNum :: Span -> Value -> Result EvalError Word64
expectNum sp (ValNum n) = Ok n
expectNum sp _          = Err (NotANumber sp)

expectFunc :: Span -> Value -> Result EvalError Func
expectFunc sp (ValFunc f) = Ok f
expectFunc sp _           = Err (NotAFunction sp)


emptyEnv :: Env
emptyEnv = M.empty

searchVar :: Env -> Span -> Text -> Result EvalError Value
searchVar env s name =
  case M.lookup name env of
    Just v  -> Ok v
    Nothing -> Err (UnknownVar s name)

-- Insert a variable into the environment
insertVar :: Text -> Value -> Env -> Env
insertVar name val env = M.insert name val env

