{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V

import           Data.Text (Text)
import           Data.Vector   (Vector)
import           Data.Word     (Word32,Word64)
import Data.Char (isAlphaNum,isAlpha,isSpace,isDigit, digitToInt)


read_file :: FilePath -> IO (Vector Text)
read_file path = do
    txt <- T.readFile path
    pure (V.fromList (T.lines txt))

sliceLine :: Text -> Int -> Int -> Text
sliceLine t a b
  | b <= a    = T.empty
  | otherwise = T.take (b - a) (T.drop a t)

data Pos = Pos
  { line :: !Word32
  , col  :: !Word32
  } deriving (Show, Eq, Ord)

data Span = Span
  { start :: !Pos
  , end   :: !Pos
  } deriving (Show, Eq, Ord)

merge :: Span -> Span -> Span
merge (Span s _) (Span _ e) = (Span s e)

print_span :: (Vector Text) -> Span -> IO ()
print_span lines' sp
  | start sp > end sp = error "bad internal spans in print_span"
  | otherwise              = mapM_ printLine [sLine .. eLine]
  where
    s       = start sp
    e       = end   sp
    sLine   = fromIntegral (line s) :: Int
    eLine   = fromIntegral (line e) :: Int
    sCol    = fromIntegral (col  s) :: Int
    eCol    = fromIntegral (col  e) :: Int

    printLine :: Int -> IO ()
    printLine i = do
      let lineText = lines' V.! i
          out
            | i == sLine && i == eLine =
                -- single-line span: [sCol, eCol)
                sliceLine lineText sCol eCol

            | i == sLine =
                -- first line of multi-line span: from sCol to end
                T.drop sCol lineText

            | i == eLine =
                -- last line of multi-line span: from start to eCol
                T.take eCol lineText

            | otherwise =
                -- middle lines: full line
                lineText

      T.putStrLn out


data CompileError
  = ParseFailed Int
  | UnexpectedToken Span
  | UnknowenName Span
  deriving (Eq)

data Input = Input {
    pos :: !Pos ,
    cur :: !Text ,
    rest :: !(Vector Text)
} deriving (Eq,Show)

new_input :: Vector Text -> Input
new_input v =
  case V.uncons v of
    Just (c, r) ->
      Input (Pos 0 0) c r

    Nothing ->
      -- no lines at all: behave as if there is one empty line
      Input (Pos 0 0) "" V.empty


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
  = (chunk, Input pos' "" rest0)

  -- consumed line; newline accepted; and we *have* a next line
  | Just nextInp <- next_line inp
  = let (more, inp') = take_while f nextInp
    in (chunk <> "\n" <> more, inp')

  -- consumed line; newline accepted; but no more input
  | otherwise
  = (chunk, Input pos' "" rest0)
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
  = Input pos' "" rest0

  -- consumed line; newline accepted; and we *have* next line
  | Just nextInp <- next_line inp
  = skip_while f nextInp

  -- consumed line; newline accepted; but end of input
  | otherwise
  = Input pos' "" rest0
  where
    (chunk, r) = T.span f cur0
    len        = T.length chunk
    pos'       = add_col len pos0


skip_space :: Input -> Input
skip_space = skip_while isSpace

try_word :: Text -> Input -> Maybe (Span, Input)
try_word target (Input pos0 cur0 rest0) = do
  curRest <- T.stripPrefix target cur0      -- if Nothing → whole function = Nothing
  let len  = T.length target
      pos1 = add_col len pos0
      sp   = Span pos0 pos1
  pure (sp, Input pos1 curRest rest0)


try_ident :: Input -> Maybe (Text, Span, Input)
try_ident inp@(Input pos0 _ _) = do
  (firstC, inp1) <- next_char inp
  if isAlpha firstC
    then do
      let (restTxt, inp2) = take_while isAlphaNum inp1
          txt = T.cons firstC restTxt
          sp  = Span pos0 (pos inp2)
      pure (txt, sp, inp2)
    else
      Nothing


try_num :: Input -> Maybe (Word64, Span, Input)
try_num inp@(Input pos0 _ _) = do
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
try_string inp@(Input pos0 _ _) = do
  (firstC, inp1) <- next_char inp
  if firstC /= '"'
    then Nothing
    else parseBody mempty inp1
  where
    parseBody acc inp@(Input _ cur _) =
      case T.uncons cur of

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


-- main :: IO ()
-- main = do
--     ls <- read_file "README.md"
--     print_span ls (Span (Pos 0 2) (Pos 0 7))

-- main :: IO ()
-- main = do
--   putStrLn "== take_while test =="

--   -- Input:
--   --   cur  = \"hello123\"
--   --   rest = [\"world!!!\", \"done\"]
--   --
--   -- Expect:
--   --   chunk = \"hello\"
--   --   remaining input starts at \"123\"
--   --
--   let testInput =
--         new_input  
--         (V.fromList ["hello123","world!!!", "done"])

--   let (chunk, Input pos' cur' rest') =
--         take_while isAlpha testInput

--   putStrLn ("chunk = " ++ show chunk)
--   putStrLn ("pos   = " ++ show pos')
--   putStrLn ("cur   = " ++ show cur')
--   putStrLn ("rest  = " ++ show rest')

--   putStrLn "\n== done =="


main :: IO ()
main = do
  putStrLn "===== Parser Streaming Test ====="

  let srcLines =
        [ "hello 123 \"hi\\nthere\""
        , "world"
        , "oops"
        ]

  let inp0 = new_input (V.fromList srcLines)
  putStrLn "\n-- Start Input --"
  print inp0


  ---------------- IDENT ----------------
  putStrLn "\ntry_ident (expect \"hello\")"
  let r1 = try_ident inp0
  print r1


  ---------------- NUM ----------------
  putStrLn "\ntry_num right after ident (expect 123)"
  let r2 = do
        (_,_,inp1) <- r1
        let inp1' = skip_space inp1
        try_num inp1'
  print r2


  ---------------- STRING ----------------
  putStrLn "\ntry_string right after number (expect \"hi\\nthere\")"
  let r3 = do
        (_,_,inp2) <- r2
        let inp2' = skip_space inp2
        try_string inp2'
  print r3


  ---------------- FAILURE TEST ----------------
  putStrLn "\ntry_num after string (expect Nothing)"
  let r4 = do
        (_,_,inp3) <- r3
        let inp3' = skip_space inp3
        try_num inp3'
  print r4


  ---------------- NEXT LINE IDENT ----------------
  putStrLn "\ntry_ident after string (expect \"world\")"
  let r5 = do
        (_,_,inp3) <- r3
        let inp3' = skip_space inp3
        try_ident inp3'
  print r5


  putStrLn "\n===== Done ====="

