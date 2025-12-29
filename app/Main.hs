{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Vector as V
import qualified Data.Text as T

import Input
import Parser
import Error
import Types  

-- showAST :: AST -> String
-- showAST = go
--   where
--     -- atom t = "(" ++ t ++ ")"

--     go (ASTNum _ n)       = show n
--     go (ASTString _ s)    = show s
--     go (ASTSymbol _ s)    = T.unpack s
--     go (ASTVoid _)        = "()"

--     -- binary ops
--     go (ASTAdd _ a b)     = "(" ++ go a ++ " + " ++ go b ++ ")"
--     go (ASTSub _ a b)     = "(" ++ go a ++ " - " ++ go b ++ ")"
--     go (ASTMul _ a b)     = "(" ++ go a ++ " * " ++ go b ++ ")"
--     go (ASTDiv _ a b)     = "(" ++ go a ++ " / " ++ go b ++ ")"

--     -- assignment
--     go (ASTAssign _ lhs rhs) =
--       "(" ++ go lhs ++ " = " ++ go rhs ++ ")"

--     -- call: fully parenthesized application
--     go (ASTCall _ xs) =
--       "(" ++ unwords (map go xs) ++ ")"


import Eval
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO (isEOF)

showValue :: Value -> String
showValue (ValNum n) = show n
showValue (ValString s) = T.unpack s
showValue ValVoid = "()"
showValue (ValFunc _) = "<function>"

repl :: Env -> IO Env
repl env = do
  eof <- isEOF
  if eof
    then pure env
    else do
      T.putStr "> "
      line <- T.getLine
      if T.null line
        then pure env
        else do
          let vec = V.fromList [line]
              inp = new_input vec
          case parse_expr inp of
            Err err -> do
              printParseError vec err
              repl env
            Ok (ast, _) -> do
              case eval env ast of
                Err evalErr -> do
                  printEvalError vec evalErr
                  repl env
                Ok (val, newEnv) -> do
                  putStrLn $ "=> " ++ showValue val
                  repl newEnv

main :: IO ()
main = do
  putStrLn "Lambda REPL v0.1"
  putStrLn "Enter expressions, or empty line to exit."
  _ <- repl emptyEnv
  pure ()