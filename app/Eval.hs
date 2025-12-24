module Eval where
import Types

eval :: AST -> Either EvalError Value
eval (ASTNum _ n) = Right (ValNum n)
eval (ASTString _ s) = Right (ValString s)
