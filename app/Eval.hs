module Eval where
import Types

eval :: AST -> Result EvalError Value
eval (ASTNum _ n) = Ok (ValNum n)
eval (ASTString _ s) = Ok (ValString s)
