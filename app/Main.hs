{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Vector as V
import qualified Data.Text as T

import Input
import Parser
import Error
import Types  

showAST :: AST -> String
showAST = go
  where
    -- atom t = "(" ++ t ++ ")"

    go (ASTNum _ n)       = show n
    go (ASTString _ s)    = show s
    go (ASTSymbol _ s)    = T.unpack s
    go (ASTVoid _)        = "()"

    -- binary ops
    go (ASTAdd _ a b)     = "(" ++ go a ++ " + " ++ go b ++ ")"
    go (ASTSub _ a b)     = "(" ++ go a ++ " - " ++ go b ++ ")"
    go (ASTMul _ a b)     = "(" ++ go a ++ " * " ++ go b ++ ")"
    go (ASTDiv _ a b)     = "(" ++ go a ++ " / " ++ go b ++ ")"

    -- assignment
    go (ASTAssign _ lhs rhs) =
      "(" ++ go lhs ++ " = " ++ go rhs ++ ")"

    -- call: fully parenthesized application
    go (ASTCall _ xs) =
      "(" ++ unwords (map go xs) ++ ")"


main :: IO ()
main = do
  putStrLn "===== Lisp Parser Test (Error Reporting) ====="

  ------------------------------------------------------------
  putStrLn "\n== OK example =="
  let okSrc =
        [ "(define (square x)"
        , "    (mul x x))"
        , "a = b + (print (square 5))"
        ]
      okVec = V.fromList okSrc
      okInp = new_input okVec

  case parse_expr okInp of
    Err err -> printParseError okVec err
    Ok (expr1, inp1) -> do
      print expr1
      case parse_expr inp1 of
        Err err -> printParseError okVec err
        Ok (expr2, _) ->
          print expr2

    ------------------------------------------------------------
  putStrLn "\n== Operator Precedence example =="

  let precSrc =
        [ "a = 1 + 2 * 3"
        , "b = (1 + 2) * 3"
        , "c = 10 - 4 - 1"
        ]
      precVec = V.fromList precSrc
      precInp = new_input precVec

  let run inp = case parse_expr inp of
        Err err -> do
          putStrLn "ERROR:"
          printParseError precVec err
          pure Nothing
        Ok (e, next) -> do
          putStrLn (showAST e)
          pure (Just next)

  r1 <- run precInp
  case r1 of
    Nothing -> pure ()
    Just i2 -> do
      r2 <- run i2
      case r2 of
        Nothing -> pure ()
        Just i3 -> do
          _ <- run i3
          pure ()


  ------------------------------------------------------------
  putStrLn "\n== UnexpectedToken example =="
  -- lone ')' token
  let utSrc =
        [ ")" ]
      utVec = V.fromList utSrc
      utInp = new_input utVec

  case parse_expr utInp of
    Err err -> printParseError utVec err
    Ok (e, _) -> print e


  ------------------------------------------------------------
  putStrLn "\n== ExpectedButGot example =="
  let ebgSrc =
        [ "123", "="]
      ebgVec = V.fromList ebgSrc
      ebgInp = new_input ebgVec

  case parse_expr ebgInp of
    Err err -> printParseError ebgVec err
    Ok (e, _) -> print e


  ------------------------------------------------------------
  putStrLn "\n== NotClosed (unclosed list) example =="
  let ncSrc =
        [ "(print (square 5"
        , "1 e"
        , "2 e"
        , "     3 e"
        , "4 e"
        ]
      ncVec = V.fromList ncSrc
      ncInp = new_input ncVec

  case parse_expr ncInp of
    Err err -> printParseError ncVec err
    Ok (e, _) -> print e


  ------------------------------------------------------------
  putStrLn "\n== UnexpectedEOF example =="
  -- completely empty program
  let eofSrc =
        [ "" ]
      eofVec = V.fromList eofSrc
      eofInp = new_input eofVec

  case parse_expr eofInp of
    Err err -> printParseError eofVec err
    Ok (e, _) -> print e


  ------------------------------------------------------------
  putStrLn "\n== Done =="