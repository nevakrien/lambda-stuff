{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Parser(
  try_expr,
  try_list,
  parse_expr,
  parse_list
)

 where

import Types
import Lexer
import Span ( mergeSpan )
import Input
import qualified Data.Text as T

try_expr :: Input -> Maybe (AST, Input)
try_expr inp = do
  (tok, sp, inp1) <- try_token inp
  case tok of
    TokenNum n      -> pure (ASTNum sp n, inp1)
    TokenString s   -> pure (ASTString sp s, inp1)
    TokenIdent s    -> pure (ASTSymbol sp s, inp1)
    TokenLParen     -> _try_list_from_open sp inp1
    TokenRParen     -> Nothing   -- unexpected ')'

try_list :: Input -> Maybe (AST, Input)
try_list = _try_list . skip_space

_try_list :: Input -> Maybe (AST, Input)
_try_list inp0 = do
  (TokenLParen, spOpen, inp1) <- try_token inp0
  _try_list_from_open spOpen inp1

_try_list_from_open :: Span -> Input -> Maybe (AST, Input)
_try_list_from_open spOpen = loop []
  where
    loop acc inp =
      case try_token inp of

        -- closing paren ends the list
        Just (TokenRParen, spClose, inp') ->
          pure (ASTList (mergeSpan spOpen spClose) (reverse acc), inp')

        -- more parse_expressions inside the list
        Just _ -> do
          (e, inp1) <- try_expr inp
          loop (e:acc) inp1

        -- EOF before ')'
        Nothing ->
          Nothing

parse_token :: Input -> Either ParseError (Token, Span, Input)
parse_token inp =
  maybe 
    (Left (UnexpectedEOF (pos inp))) 
    Right (try_token inp)

parse_expr :: Input -> Either ParseError (AST, Input)
parse_expr inp = do
  (tok, sp, inp1) <- parse_token inp
  case tok of
    TokenNum n    -> Right (ASTNum sp n, inp1)
    TokenString s -> Right (ASTString sp s, inp1)
    TokenIdent s  -> Right (ASTSymbol sp s, inp1)
    TokenLParen   -> parse_list_from_open sp inp1
    TokenRParen   -> Left (UnexpectedToken sp )

parse_list :: Input -> Either ParseError (AST, Input)
parse_list = listStartE . skip_space

listStartE :: Input -> Either ParseError (AST, Input)
listStartE inp0 = do
  (tok, spOpen, inp1) <- parse_token inp0
  case tok of
    TokenLParen -> parse_list_from_open spOpen inp1
    _         -> Left (ExpectedButGot (T.pack "'('") spOpen )

parse_list_from_open :: Span -> Input -> Either ParseError (AST, Input)
parse_list_from_open spOpen = loop []
  where
    loop acc inp =
      case try_token inp of
        -- closing paren ends list
        Just (TokenRParen, spClose, inp') ->
          Right (ASTList (mergeSpan spOpen spClose) (reverse acc), inp')

        -- definitely another parse_expression
        Just _ -> do
          (e, inp1) <- parse_expr inp
          loop (e:acc) inp1

        -- EOF before ')'
        Nothing ->
          Left (NotClosed spOpen (pos inp))