{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Vector as V

import Input
import Parser
import Error

main :: IO ()
main = do
  putStrLn "===== Lisp Parser Test (Error Reporting) ====="

  ------------------------------------------------------------
  putStrLn "\n== OK example =="
  let okSrc =
        [ "(define (square x) (mul x x))"
        , "(print (square 5))"
        ]
      okVec = V.fromList okSrc
      okInp = new_input okVec

  case parse_expr okInp of
    Left err -> printParseError okVec err
    Right (expr1, inp1) -> do
      print expr1
      case parse_expr inp1 of
        Left err -> printParseError okVec err
        Right (expr2, _) ->
          print expr2


  ------------------------------------------------------------
  putStrLn "\n== UnexpectedToken example =="
  -- lone ')' token
  let utSrc =
        [ ")" ]
      utVec = V.fromList utSrc
      utInp = new_input utVec

  case parse_expr utInp of
    Left err -> printParseError utVec err
    Right (e, _) -> print e


  ------------------------------------------------------------
  putStrLn "\n== ExpectedButGot example =="
  -- calling parse_list explicitly on something that's NOT '('
  let ebgSrc =
        [ "123" ]
      ebgVec = V.fromList ebgSrc
      ebgInp = new_input ebgVec

  case parse_list ebgInp of
    Left err -> printParseError ebgVec err
    Right (e, _) -> print e


  ------------------------------------------------------------
  putStrLn "\n== NotCLose (unclosed list) example =="
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
    Left err -> printParseError ncVec err
    Right (e, _) -> print e


  ------------------------------------------------------------
  putStrLn "\n== UnexpectedEOF example =="
  -- completely empty program
  let eofSrc =
        [ "" ]
      eofVec = V.fromList eofSrc
      eofInp = new_input eofVec

  case parse_expr eofInp of
    Left err -> printParseError eofVec err
    Right (e, _) -> print e


  ------------------------------------------------------------
  putStrLn "\n== Done =="