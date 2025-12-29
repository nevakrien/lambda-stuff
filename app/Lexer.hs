{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Lexer where

import Types
import Input
import Data.Char (isAlphaNum,isAlpha,isDigit, digitToInt)
import Data.Word (Word64)
import qualified Data.Text as T
import Data.Text (Text)

try_word :: Text -> Input -> Maybe (Span, Input)
try_word target = _try_word target . skip_space

_try_word :: Text -> Input -> Maybe (Span, Input)
_try_word target (Input pos0 cur0 rest0) = do
  curRest <- T.stripPrefix target cur0      -- if Nothing → whole function = Nothing
  let len  = T.length target
      pos1 = addCol len pos0
      sp   = Span pos0 pos1
  pure (sp, Input pos1 curRest rest0)

try_ident :: Input -> Maybe (Text, Span, Input)
try_ident = _try_ident . skip_space

_try_ident :: Input -> Maybe (Text, Span, Input)
_try_ident inp@(Input pos0 _ _) = do
  (firstC, inp1) <- next_char inp
  if isAlpha firstC
    then do
      let (restTxt, inp2) = take_while isAlphaNum inp1
          txt = T.cons firstC restTxt
          sp  = Span pos0 (pos inp2)
      pure (txt, sp, inp2)
    else
      Nothing

_try_prefixed_ident :: Input -> Maybe (Text, Span, Input)
_try_prefixed_ident inp@(Input pos0 _ _) = do
  (_, inp1) <- next_char inp
  (t,_,inp2) <- _try_ident inp1
  let s = Span pos0 (pos inp2)
  pure(t, s, inp2)


try_num :: Input -> Maybe (Word64, Span, Input)
try_num = _try_num . skip_space

_try_num :: Input -> Maybe (Word64, Span, Input)
_try_num inp@(Input pos0 _ _) = do
  (firstC, inp1) <- next_char inp
  if not (isDigit firstC)
    then Nothing
    else do
      let (restTxt, inp2) = take_while isDigit inp1
          fullTxt = T.cons firstC restTxt
      value <- textToWord64 fullTxt
      let sp = Span pos0 (pos inp2)
      pure (value, sp, inp2)

textToWord64 :: Text -> Maybe Word64
textToWord64 = T.foldl' step (Just 0)
  where
    step acc ch = do
      x <- acc
      let d = fromIntegral (digitToInt ch)
      let y = x * 10 + d
      if y < x      -- overflow detection
        then Nothing
        else Just y

try_string :: Input -> Maybe (Text, Span, Input)
try_string = _try_string . skip_space

_try_string :: Input -> Maybe (Text, Span, Input)
_try_string inp0@(Input pos0 _ _) = do
  (firstC, inp1) <- next_char inp0
  if firstC /= '"'
    then Nothing
    else parseBody mempty inp1
  where
    parseBody acc inp =
      case T.uncons (cur inp) of

        -- end of string
        Just ('"', _) -> do
          (_, inpDone) <- next_char inp
          let sp = Span pos0 (pos inpDone)
          pure (acc, sp, inpDone)

        -- escape sequence
        Just ('\\', _) -> do
          (_, inpEsc) <- next_char inp
          (c, inpNext) <- parseEscape inpEsc
          parseBody (T.snoc acc c) inpNext

        -- newline before closing → invalid
        Just ('\n', _) -> Nothing

        -- normal char
        Just (c, _) -> do
          (_, inpNext) <- next_char inp
          parseBody (T.snoc acc c) inpNext

        -- EOF before closing
        Nothing -> Nothing

parseEscape :: Input -> Maybe (Char, Input)
parseEscape inp = do
  (c, inp1) <- next_char inp
  case c of
    '"'  -> pure ('"', inp1)
    '\\' -> pure ('\\', inp1)
    'n'  -> pure ('\n', inp1)
    't'  -> pure ('\t', inp1)
    'r'  -> pure ('\r', inp1)
    _    -> Nothing

try_token :: Input -> Maybe (Token, Span, Input)
try_token inp0 =
  let inp = skip_space inp0
  in case T.uncons (cur inp) of
       Just (c, _) 
          | isAlpha c ->
              do (txt, sp, inp') <- _try_ident inp
                 case txt of
                  "if"     -> pure (TokenIf, sp, inp')
                  "then"   -> pure (TokenThen, sp, inp')
                  "else"   -> pure (TokenElse, sp, inp')
                  _ -> pure (TokenIdent txt, sp, inp')

          | isDigit c ->
              do (n, sp, inp') <- _try_num inp
                 pure (TokenNum n, sp, inp')

          | c == '"' ->
              do (txt, sp, inp') <- _try_string inp
                 pure (TokenString txt, sp, inp')

          | c == '(' -> consume1 TokenLParen inp
          | c == ')' -> consume1 TokenRParen inp
          | c == '=' -> consume1 TokenEqual  inp
          | c == '+' -> consume1 TokenPlus   inp
          | c == '-' -> consume1 TokenMinus inp
          | c == '*' -> consume1 TokenStar   inp
          | c == '/' -> case _try_prefixed_ident inp of
              Just(txt,s,inp') -> 
                pure (TokenLambdaVar txt, s, inp')
              Nothing -> consume1 TokenSlash   inp
          
          | c == '\\' -> case _try_prefixed_ident inp of
              Just(txt,s,inp') -> 
                pure (TokenLambdaVar txt, s, inp')
              Nothing -> consume1 (TokenInvalid c) inp
          | otherwise -> consume1 (TokenInvalid c) inp
       _ -> Nothing
  where
    consume1 :: Token -> Input -> Maybe (Token, Span, Input)
    consume1 tok i = do
      (_, i') <- next_char i
      let sp = Span (pos i) (pos i')
      pure (tok, sp, i')

third :: (a,b,c) -> c
third (_,_,x) = x
