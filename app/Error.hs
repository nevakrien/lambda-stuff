{-# LANGUAGE OverloadedStrings #-}
module Error where

import Types
import Span
import qualified Data.Text as T
import Data.Text (Text)
import Data.Vector (Vector)


ansiReset :: String
ansiReset = "\ESC[0m"

-- style for the text: "error: something something"
errorMessageStyle :: String
errorMessageStyle = "\ESC[31m"          -- currently dark red

-- style for the highlighted offending text
errorHighlightStyle :: String
errorHighlightStyle = "\ESC[31m"        -- currently same dark red



printParseError :: Vector Text -> ParseError -> IO ()
printParseError src err = do
  putStrLn ("at " ++ showLineCol posLine posCol)

  case err of
    UnexpectedEOF _ -> do
      putStrLn (errorMessageStyle ++ "error: unexpected end of input" ++ ansiReset)
      putStrLn ""
      putStrLn "parser was here when input ended."

    UnexpectedToken sp -> do
      putStrLn (errorMessageStyle ++ "error: unexpected token:" ++ ansiReset)
      renderSpanContext src sp

    ExpectedButGot expected sp -> do
      putStrLn (errorMessageStyle
        ++ "error: expected " ++ T.unpack expected ++ ", but got:"
        ++ ansiReset)
      renderSpanContext src sp

    NotCLose openSp endPos -> do
      putStrLn (errorMessageStyle ++ "error: list opened here but never closed:" ++ ansiReset)
      renderSpanContextExtra src openSp endPos



  where
    (posLine, posCol) =
      case err of
        UnexpectedEOF (Pos l c) -> (l, c)
        UnexpectedToken sp       -> spanStart (start sp)
        ExpectedButGot _ sp      -> spanStart (start sp)
        NotCLose openSp _        -> spanStart (start openSp)

    spanStart (Pos l c) = (l, c)

    showLineCol l c =
      "line " ++ show (fromIntegral l + 1 :: Int)
      ++ ", column " ++ show (fromIntegral c + 1 :: Int)


renderSpanContext :: Vector Text -> Span -> IO ()
renderSpanContext src sp = do
  let (pre, mid, post) = spanWithContext src sp
  renderStyledError pre mid post

renderStyledError :: Text -> Text -> Text -> IO ()
renderStyledError pre mid post = do
  putStrLn $
       T.unpack pre
    ++ errorHighlightStyle
    ++ T.unpack mid
    ++ ansiReset
    ++ T.unpack post


renderSpanContextExtra :: Vector Text -> Span -> Pos -> IO ()
renderSpanContextExtra src sp endPos = do
  -- normal highlight split
  let (pre, mid, _) = spanWithContext src sp

  -- span from open start → end position
  let (_, midToEnd0, _) =
        spanWithContext src (Span (start sp) endPos)

  putStrLn $
       T.unpack pre
    ++ errorHighlightStyle
    ++ T.unpack mid
    ++ ansiReset
    ++ clipEnd 1000 midToEnd0
    -- ++ clipEnd 1000 midToEnd1



clipEnd :: Int -> Text -> String
clipEnd maxLen txt
  | T.length txt <= maxLen = T.unpack txt
  | otherwise =
      let len = T.length txt
      in "\ESC[35m...\n\ESC[0m" ++ T.unpack (T.drop (len - maxLen) txt)

