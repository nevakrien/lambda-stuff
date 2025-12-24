{-# LANGUAGE OverloadedStrings #-}
module Span where

import Types
import qualified Data.Text as T
-- import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Data.Text (Text)
import Data.Vector (Vector)

merge_span :: Span -> Span -> Span
merge_span (Span s _) (Span _ e) = (Span s e)

sliceLine :: Text -> Int -> Int -> Text
sliceLine t a b
  | b <= a    = T.empty
  | otherwise = T.take (b - a) (T.drop a t)

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
