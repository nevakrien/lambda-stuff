{-# LANGUAGE OverloadedStrings #-}
module Span (
  spanWithContext, spanText, mergeSpan
  ,spanOf, withSpan
)
where

import Types
import qualified Data.Text as T
-- import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Data.Text (Text)
import Data.Vector (Vector)

mergeSpan :: Span -> Span -> Span
mergeSpan (Span s _) (Span _ e) = Span s e

spanOf :: AST -> Span
spanOf (ASTNum    sp _)   = sp
spanOf (ASTString sp _)   = sp
spanOf (ASTSymbol sp _)   = sp
spanOf (ASTCall   sp _)   = sp
spanOf (ASTAssign sp _ _) = sp
spanOf (ASTAdd    sp _ _) = sp
spanOf (ASTSub    sp _ _) = sp
spanOf (ASTMul    sp _ _) = sp
spanOf (ASTDiv    sp _ _) = sp
spanOf (ASTVoid   sp )    = sp
spanOf (ASTFunc  sp _ _ _) = sp
spanOf (ASTIf  sp _ _ _) = sp

withSpan :: Span -> AST -> AST
withSpan sp (ASTNum    _ n)   = ASTNum    sp n
withSpan sp (ASTString _ s)   = ASTString sp s
withSpan sp (ASTSymbol _ s)   = ASTSymbol sp s  
withSpan sp (ASTCall   _ xs)  = ASTCall   sp xs
withSpan sp (ASTAssign _ a b) = ASTAssign sp a b
withSpan sp (ASTAdd    _ a b) = ASTAdd    sp a b
withSpan sp (ASTSub    _ a b) = ASTSub    sp a b
withSpan sp (ASTMul    _ a b) = ASTMul    sp a b
withSpan sp (ASTDiv    _ a b) = ASTDiv    sp a b
withSpan sp (ASTVoid   _ )    = ASTVoid   sp
withSpan sp (ASTFunc  _ body argSpan argName) = ASTFunc sp body argSpan argName
withSpan sp (ASTIf  _ cond thenBr elseBr) = ASTIf sp cond thenBr elseBr

-- sliceLine :: Text -> Int -> Int -> Text
-- sliceLine t a b
--   | b <= a    = T.empty
--   | otherwise = T.take (b - a) (T.drop a t)

spanWithContext
  :: Vector Text
  -> Span
  -> (Text, Text, Text)   -- (pre, mid, post)
spanWithContext v sp
  | start sp > end sp = (T.empty, T.empty, T.empty)
  | sLine == eLine =
      let line' = v V.! sLine
          (pre, rest') = T.splitAt sCol line'
          (mid, post) = T.splitAt (eCol - sCol) rest'
      in (pre, mid, post)

  | otherwise =
      let lineS = v V.! sLine
          lineE = v V.! eLine

          (pre,   midHead) = T.splitAt sCol lineS
          (midTail, post)    = T.splitAt eCol lineE

          middle =
            [ v V.! i | i <- [sLine+1 .. eLine-1] ]

          mid = T.intercalate "\n" (midHead : middle ++ [midTail])
      in (pre, mid, post)

  where
    s     = start sp
    e     = end sp
    sLine = fromIntegral (line s)
    eLine = fromIntegral (line e)
    sCol  = fromIntegral (col  s)
    eCol  = fromIntegral (col  e)


spanText :: Vector Text -> Span -> Text
spanText v sp =
  let (_, ans, _) = spanWithContext v sp
  in ans
