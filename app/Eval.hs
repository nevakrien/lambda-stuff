-- module Eval(Env,eval) where
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Eval(Env,eval,searchVar,emptyEnv) where
import Types
import Span
import qualified Data.Map.Strict as M
import Data.Foldable (asum)
import Data.Text (Text)
import Data.Word (Word64)

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
    divOp (ValNum a) _ _ spb = Err (NotANumber spb)
    divOp _ spa _ _ = Err (NotANumber spa)

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


runFunc :: Func -> [Value] -> Result EvalError Value
runFunc _ _ = Ok ValVoid

-- evalFunc :: Env -> AST -> Result EvalError Func
-- evalFunc env x =
--     case eval env x of
--       Ok (ValFunc f) -> Ok f
--       Ok _           -> Err (NotAFunction (spanOf x))
--       Err e          -> Err e

-- One frame = local scope
type Frame = M.Map Text Value

-- Environment is stack of frames
newtype Env = Env [Frame]
  deriving (Show)

emptyEnv :: Env
emptyEnv = Env [M.empty]

searchVar :: Env -> Span -> Text -> Result EvalError Value
searchVar (Env frames) s name =
  case asum (map (M.lookup name) frames) of
    Just v  -> Ok v
    Nothing -> Err (UnknownVar s name)

-- Insert into top frame
insertVar :: Text -> Value -> Env -> Env
insertVar name val (Env (f:fs)) =
    Env (M.insert name val f : fs)
insertVar _ _ (Env []) =
    error "Env invariant broken: no frames"

-- Push a new lexical scope
pushFrame :: Env -> Env
pushFrame (Env frames) = Env (M.empty : frames)

-- Pop scope
popFrame :: Env -> Env
popFrame (Env (_:fs)) = Env fs
popFrame (Env []) =
    error "Env invariant broken: no frames"



