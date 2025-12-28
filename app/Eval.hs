-- module Eval(Env,eval) where
module Eval(Env,searchVar,emptyEnv) where
import Types
import Span
import qualified Data.Map.Strict as M
import Data.Foldable (asum)
import Data.Text (Text)

-- eval :: Env -> AST -> Result EvalError Value
  

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



