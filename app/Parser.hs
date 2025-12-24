module Parser where

import Types
import Lexer
import Span
import Input
import qualified Data.Text as T

try_expr :: Input -> Maybe (Expr, Input)
try_expr inp = do
  (tok, sp, inp1) <- try_token inp
  case tok of
    TokNum n      -> pure (ENum sp n, inp1)
    TokString s   -> pure (EString sp s, inp1)
    TokIdent s    -> pure (ESymbol sp s, inp1)
    TokLParen     -> _try_list_from_open sp inp1
    TokRParen     -> Nothing   -- unexpected ')'

try_list :: Input -> Maybe (Expr, Input)
try_list = _try_list . skip_space

_try_list :: Input -> Maybe (Expr, Input)
_try_list inp0 = do
  (TokLParen, spOpen, inp1) <- try_token inp0
  _try_list_from_open spOpen inp1

_try_list_from_open :: Span -> Input -> Maybe (Expr, Input)
_try_list_from_open spOpen = loop []
  where
    loop acc inp =
      case try_token inp of

        -- closing paren ends the list
        Just (TokRParen, spClose, inp') ->
          pure (EList (merge_span spOpen spClose) (reverse acc), inp')

        -- more parse_exprssions inside the list
        Just _ -> do
          (e, inp1) <- try_expr inp
          loop (e:acc) inp1

        -- EOF before ')'
        Nothing ->
          Nothing

parse_token :: Input -> Either ParseError (Token, Span, Input)
parse_token inp =
  case try_token inp of
    Just r -> Right r
    Nothing -> (Left (UnexpectedEOF (pos inp)))

parse_expr :: Input -> Either ParseError (Expr, Input)
parse_expr inp = do
  (tok, sp, inp1) <- parse_token inp
  case tok of
    TokNum n    -> Right (ENum sp n, inp1)
    TokString s -> Right (EString sp s, inp1)
    TokIdent s  -> Right (ESymbol sp s, inp1)
    TokLParen   -> parse_list_from_open sp inp1
    TokRParen   -> Left (UnexpectedToken sp )

parse_list :: Input -> Either ParseError (Expr, Input)
parse_list = listStartE . skip_space

listStartE :: Input -> Either ParseError (Expr, Input)
listStartE inp0 = do
  (tok, spOpen, inp1) <- parse_token inp0
  case tok of
    TokLParen -> parse_list_from_open spOpen inp1
    _         -> Left (ExpectedButGot (T.pack "'('") spOpen )

parse_list_from_open :: Span -> Input -> Either ParseError (Expr, Input)
parse_list_from_open spOpen = loop []
  where
    loop acc inp =
      case try_token inp of
        -- closing paren ends list
        Just (TokRParen, spClose, inp') ->
          Right (EList (merge_span spOpen spClose) (reverse acc), inp')

        -- definitely another parse_exprssion
        Just _ -> do
          (e, inp1) <- parse_expr inp
          loop (e:acc) inp1

        -- EOF before ')'
        Nothing ->
          Left (NotCLose spOpen (pos inp))