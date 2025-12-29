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
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Control.Monad (when)

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

-- Load a file into environment (used for --repl mode)
loadFile :: FilePath -> IO (Either EvalError Env)
loadFile filePath = do
  exists <- doesFileExist filePath
  if not exists
    then do
      putStrLn $ "Error: File '" ++ filePath ++ "' does not exist."
      exitFailure
    else do
      vec <- read_file filePath
      execFile vec emptyEnv

-- Execute expressions from a file
runFile :: FilePath -> IO ()
runFile filePath = do
  exists <- doesFileExist filePath
  if not exists
    then do
      putStrLn $ "Error: File '" ++ filePath ++ "' does not exist."
      exitFailure
    else do
      vec <- read_file filePath
      result <- execFile vec emptyEnv
      case result of
        Left err -> do
          printEvalError vec err
          exitFailure
        Right _ -> pure ()

-- Execute all expressions in a file by parsing the entire input
execFile :: V.Vector T.Text -> Env -> IO (Either EvalError Env)
execFile vec = loop (new_input vec)
  where
    loop inp env =
      let cur'  = cur inp
          rest' = rest inp
      in if T.null cur' && V.null rest'
         then pure (Right env)
         else
           case parse_expr inp of
             Err perr -> do
               printParseError vec perr
               exitFailure
             Ok (ast, inp') -> do
               case eval env ast of
                 Err e -> pure (Left e)
                 Ok (val, env') -> do
                   when (val /= ValVoid) $
                     putStrLn ("=> " ++ showValue val)
                   loop inp' env'


main :: IO ()
main = do
  args <- getArgs
  let hasReplFlag = "--repl" `elem` args
      nonFlagArgs = filter (/= "--repl") args
  case args of
    [] -> do
      putStrLn "Lambda REPL v0.1"
      putStrLn "Enter expressions, or empty line to exit."
      _ <- repl emptyEnv
      pure ()
    ["--help"] -> do
      putStrLn "Lambda Stuff - A Simple Lambda Calculus Interpreter"
      putStrLn ""
      putStrLn "Usage:"
      putStrLn "  lambda-stuff           # Start REPL"
      putStrLn "  lambda-stuff <file>    # Execute file"
      putStrLn "  lambda-stuff --repl <file>  # Start REPL after loading file"
      putStrLn "  lambda-stuff <file> --repl  # Start REPL after loading file"
      putStrLn "  lambda-stuff --help    # Show this help"
      putStrLn ""
      putStrLn "File Format:"
      putStrLn "  Files should contain one expression per line."
      putStrLn "  Lines starting with '#' are treated as comments."
      putStrLn "  Empty lines are ignored."
      putStrLn ""
      putStrLn "Examples:"
      putStrLn "  lambda-stuff                    # Start interactive REPL"
      putStrLn "  lambda-stuff program.lambda     # Run a program file"
      putStrLn "  lambda-stuff --repl lib.lambda # Load library and start REPL"
      putStrLn "  lambda-stuff lib.lambda --repl # Load library and start REPL"
      pure ()
    _ | hasReplFlag && length nonFlagArgs == 1 -> do
      let filePath = head nonFlagArgs
      putStrLn $ "Loading file: " ++ filePath
      result <- loadFile filePath
      case result of
        Left err -> do
          vec <- read_file filePath
          let filteredLines = V.filter (\l -> not (T.null l || T.isPrefixOf "#" l)) vec
          printEvalError filteredLines err
          exitFailure
        Right env -> do
          putStrLn "File loaded successfully. Starting REPL..."
          _ <- repl env
          pure ()
    _ | not hasReplFlag && length nonFlagArgs == 1 -> do
      let filePath = head nonFlagArgs
      putStrLn $ "Executing file: " ++ filePath
      runFile filePath
    _ -> do
      putStrLn "Usage:"
      putStrLn "  lambda-stuff           # Start REPL"
      putStrLn "  lambda-stuff <file>    # Execute file"
      putStrLn "  lambda-stuff --repl <file>  # Start REPL after loading file"
      putStrLn "  lambda-stuff <file> --repl  # Start REPL after loading file"
      putStrLn "  lambda-stuff --help    # Show help"
      exitFailure