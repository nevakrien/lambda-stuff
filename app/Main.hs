{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Vector as V

import Input
import Parser
import Error
import Types  

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
    Err err -> printParseError okVec err
    Ok (expr1, inp1) -> do
      print expr1
      case parse_expr inp1 of
        Err err -> printParseError okVec err
        Ok (expr2, _) ->
          print expr2


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
  -- calling parse_list explicitly on something that's NOT '('
  let ebgSrc =
        [ "123" ]
      ebgVec = V.fromList ebgSrc
      ebgInp = new_input ebgVec

  case parse_list ebgInp of
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