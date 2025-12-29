{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Parser(
  parse_expr,
)

where

import Types
import Lexer ( try_token )  
import Span ( mergeSpan, spanOf, withSpan )

parse_token :: Input -> Result ParseError (Token, Span, Input)
parse_token inp =
  maybe 
    (Err (UnexpectedEOF (pos inp))) 
    Ok (try_token inp)



-- classifier:
--   given a token returns a node builder
--   otherwise Nothing means "this token is not a binop here"
type BinClassifier =
  Token -> Maybe (Span -> AST -> AST -> AST)

parse_bin
  :: BinClassifier
  -> (Input -> Result ParseError (AST, Input))
  -> Input
  -> Result ParseError (AST, Input)
parse_bin classify parse_inner inp0 = do
  (base, inp1) <- parse_inner inp0
  loop base inp1
  where
    loop lhs inp =
      case try_token inp of
        Just (tok, _, inp2)
          | Just mkNode <- classify tok -> do
              (rhs, inp3) <- parse_inner inp2
              let s   = mergeSpan (spanOf lhs) (spanOf rhs)
                  ast = mkNode s lhs rhs
              loop ast inp3          -- keep chaining (left associative)

        _ -> Ok (lhs, inp)          -- no matching operator → stop


parse_expr :: Input -> Result ParseError (AST, Input)
parse_expr = parse_assign 

parse_assign :: Input -> Result ParseError (AST, Input)
parse_assign =
  parse_bin classify parse_rexp
  where
    classify TokenEqual = Just ASTAssign
    classify _          = Nothing


parse_rexp :: Input -> Result ParseError (AST, Input)
parse_rexp = parse_add

parse_add :: Input -> Result ParseError (AST, Input)
parse_add =
  parse_bin classify parse_mul
  where
    classify TokenPlus  = Just ASTAdd
    classify TokenMinus = Just ASTSub
    classify _          = Nothing


parse_mul :: Input -> Result ParseError (AST, Input)
parse_mul =
  parse_bin classify parse_atom
  where
    classify TokenStar = Just ASTMul
    classify TokenSlash = Just ASTDiv
    classify _ = Nothing



parse_atom :: Input -> Result ParseError (AST, Input)
parse_atom inp = do
  (tok, sp, inp1) <- parse_token inp
  case tok of
    TokenNum n    -> Ok (ASTNum sp n, inp1)
    TokenString s -> Ok (ASTString sp s, inp1)
    TokenIdent s  -> Ok (ASTSymbol sp s, inp1)
    TokenLParen   -> do 
      (x, inp2) <- parse_list_from_open sp inp1
      Ok (disambiguate_list x, inp2)

    TokenLambdaVar argName -> do
      (body, inp2) <- parse_rexp inp1
      let full_sp = mergeSpan sp (spanOf body)
      let funcAst = ASTFunc full_sp body sp argName
      Ok (funcAst, inp2)
    _   -> Err (UnexpectedToken sp )


disambiguate_list :: AST -> AST
disambiguate_list (ASTCall sp []) = ASTVoid sp 
disambiguate_list (ASTCall sp [x]) = withSpan sp x 
disambiguate_list x = x

parse_list_from_open :: Span -> Input -> Result ParseError (AST, Input)
parse_list_from_open spOpen = loop []
  where
    loop acc inp =
      case try_token inp of
        -- closing paren ends list
        Just (TokenRParen, spClose, inp') ->
          Ok (ASTCall (mergeSpan spOpen spClose) (reverse acc), inp')

        -- definitely another parse_expression
        Just _ -> do
          (e, inp1) <- parse_rexp inp
          loop (e:acc) inp1

        -- EOF before ')'
        Nothing ->
          Err (NotClosed spOpen (pos inp))



