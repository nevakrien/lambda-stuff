module Input where

import Types
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Char (isSpace)
import Data.Word (Word32)

read_file :: FilePath -> IO (Vector Text)
read_file path = do
    txt <- T.readFile path
    pure (V.fromList (T.lines txt))

new_input :: Vector Text -> Input
new_input v =
  case V.uncons v of
    Just (c, r) ->
      Input (Pos 0 0) c r

    Nothing ->
      -- no lines at all: behave as if there is one empty line
      Input (Pos 0 0) T.empty V.empty

-- checked add that fits in 32
addIndex :: Int -> Word32 -> Word32
addIndex n w
  | x > fromIntegral (maxBound :: Word32)
      = error "input too large"
  | otherwise = fromIntegral x
  where
    x :: Int
    x = fromIntegral w + n

next_line :: Input -> Maybe Input
next_line (Input (Pos l _ ) _ rest') = do
  (next, rest'') <- V.uncons rest'
  pure (Input (Pos (addIndex 1 l) 0) next rest'')

add_col :: Int -> Pos -> Pos 
add_col n (Pos l c) = Pos l (addIndex n c)

inc_col :: Pos -> Pos 
inc_col = add_col 1

next_char :: Input -> Maybe ( Char,Input )
next_char inp@(Input pos' cur' rest') =
  case T.uncons cur' of
    Just (c, next') ->
      Just (c, Input (inc_col pos') next' rest')

    Nothing -> do
      inp' <- next_line inp
      next_char inp'

take_while :: (Char -> Bool) -> Input -> (Text, Input)
take_while f inp@(Input pos0 cur0 rest0)
  -- stopped mid-line
  | not (T.null r)
  = (chunk, Input pos' r rest0)

  -- consumed line; newline rejected
  | not (f '\n')
  = (chunk, Input pos' T.empty rest0)

  -- consumed line; newline accepted; and we *have* a next line
  | Just nextInp <- next_line inp
  = let (more, inp') = take_while f nextInp
    in (chunk <> T.pack "\n" <> more, inp')

  -- consumed line; newline accepted; but no more input
  | otherwise
  = (chunk, Input pos' T.empty rest0)
  where
    (chunk, r) = T.span f cur0
    len        = T.length chunk
    pos'       = add_col len pos0

skip_while :: (Char -> Bool) -> Input -> Input
skip_while f inp@(Input pos0 cur0 rest0)
  -- stopped mid-line
  | not (T.null r)
  = Input pos' r rest0

  -- consumed line; newline rejected
  | not (f '\n')
  = Input pos' T.empty rest0

  -- consumed line; newline accepted; and we *have* next line
  | Just nextInp <- next_line inp
  = skip_while f nextInp

  -- consumed line; newline accepted; but end of input
  | otherwise
  = Input pos' T.empty rest0
  where
    (chunk, r) = T.span f cur0
    len        = T.length chunk
    pos'       = add_col len pos0

skip_space :: Input -> Input
skip_space = skip_while isSpace