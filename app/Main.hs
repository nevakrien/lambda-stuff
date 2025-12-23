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

merge_span :: Span -> Span -> Span
merge_span (Span s _) (Span _ e) = (Span s e)

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
try_word target = _try_word target . skip_space

_try_word :: Text -> Input -> Maybe (Span, Input)
_try_word target (Input pos0 cur0 rest0) = do
  curRest <- T.stripPrefix target cur0      -- if Nothing → whole function = Nothing
  let len  = T.length target
      pos1 = add_col len pos0
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


data Token
  = TokIdent  Text
  | TokNum    Word64
  | TokString Text
  | TokLParen
  | TokRParen
  deriving (Show, Eq)

try_token :: Input -> Maybe (Token, Span, Input)
try_token inp0 =
  let inp = skip_space inp0
  in case T.uncons (cur inp) of
       Just (c, _) 
         | isAlpha c ->
             do (txt, sp, inp') <- _try_ident inp
                pure (TokIdent txt, sp, inp')

         | isDigit c ->
             do (n, sp, inp') <- _try_num inp
                pure (TokNum n, sp, inp')

         | c == '"' ->
             do (txt, sp, inp') <- _try_string inp
                pure (TokString txt, sp, inp')

         | c == '(' ->
             do (_, inp') <- next_char inp
                let sp = Span (pos inp) (pos inp')
                pure (TokLParen, sp, inp')

         | c == ')' ->
             do (_, inp') <- next_char inp
                let sp = Span (pos inp) (pos inp')
                pure (TokRParen, sp, inp')

       _ -> Nothing

data Expr
  = ENum    Span Word64
  | EString Span Text
  | ESymbol Span Text
  | EList   Span [Expr]
  deriving (Show, Eq)

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


data ParseError
  = UnexpectedEOF Pos
  | UnexpectedToken Span
  | ExpectedButGot Text Span
  | NotCLose Span Pos --start end
  deriving (Show, Eq)

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
    _         -> Left (ExpectedButGot "'('" spOpen )


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


spanText :: Vector Text -> Span -> Text
spanText vlines sp
  | start sp > end sp = ""
  | sLine == eLine =
      sliceLine (vlines V.! sLine) sCol eCol
  | otherwise =
      T.intercalate "\n"
        [ sliceLine (vlines V.! sLine) sCol (T.length (vlines V.! sLine))
        , T.intercalate "\n"
            [ vlines V.! i | i <- [sLine+1 .. eLine-1] ]
        , T.take eCol (vlines V.! eLine)
        ]
  where
    s = start sp
    e = end sp
    sLine = fromIntegral (line s)
    eLine = fromIntegral (line e)
    sCol  = fromIntegral (col  s)
    eCol  = fromIntegral (col  e)


printParseError :: Vector Text -> ParseError -> IO ()
printParseError src err = do
  putStrLn ("at " ++ showLineCol posLine posCol)
  case err of

    UnexpectedEOF _ -> do
      putStrLn "error: unexpected end of input"
      putStrLn ""
      putStrLn "parser was here when input ended."

    UnexpectedToken sp -> do
      putStrLn "error: unexpected token:"
      print_span src sp
      pointerLine sp

    ExpectedButGot expected sp -> do
      putStrLn ("error: expected " ++ T.unpack expected ++ ", but got:")
      print_span src sp
      pointerLine sp

    NotCLose openSp _ -> do
      putStrLn "error: list opened here but never closed:"
      print_span src openSp
      pointerLine openSp
  where
    -- pick a representative position for the error to report
    (posLine, posCol) =
      case err of
        UnexpectedEOF (Pos l c) -> (l, c)
        UnexpectedToken sp       -> spanStart (start sp)
        ExpectedButGot _ sp      -> spanStart (start sp)
        NotCLose openSp _        -> spanStart (start openSp)

    spanStart (Pos l c) = (l, c)

    -- user-facing, 1-based indices
    showLineCol l c =
      "line " ++ show (fromIntegral l + 1 :: Int)
      ++ ", column " ++ show (fromIntegral c + 1 :: Int)


-- underline a span (only if single-line)
pointerLine :: Span -> IO ()
pointerLine sp
  | line s /= line e = pure ()
  | otherwise =
      putStrLn $
        replicate startC ' ' ++ replicate width '^'
  where
    s = start sp
    e = end   sp
    startC = fromIntegral (col s)
    endC   = fromIntegral (col e)
    width  = max 1 (endC - startC)



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
    Left err -> printParseError okVec err
    Right (expr1, inp1) -> do
      print expr1
      case parse_expr inp1 of
        Left err -> printParseError okVec err
        Right (expr2, _) ->
          print expr2


  ------------------------------------------------------------
  putStrLn "\n== UnexpectedToken example =="
  -- lone ')' token
  let utSrc =
        [ ")" ]
      utVec = V.fromList utSrc
      utInp = new_input utVec

  case parse_expr utInp of
    Left err -> printParseError utVec err
    Right (e, _) -> print e


  ------------------------------------------------------------
  putStrLn "\n== ExpectedButGot example =="
  -- calling parse_list explicitly on something that’s NOT '('
  let ebgSrc =
        [ "123" ]
      ebgVec = V.fromList ebgSrc
      ebgInp = new_input ebgVec

  case parse_list ebgInp of
    Left err -> printParseError ebgVec err
    Right (e, _) -> print e


  ------------------------------------------------------------
  putStrLn "\n== NotCLose (unclosed list) example =="
  let ncSrc =
        [ "(print (square 5"
        ]
      ncVec = V.fromList ncSrc
      ncInp = new_input ncVec

  case parse_expr ncInp of
    Left err -> printParseError ncVec err
    Right (e, _) -> print e


  ------------------------------------------------------------
  putStrLn "\n== UnexpectedEOF example =="
  -- completely empty program
  let eofSrc =
        [ "" ]
      eofVec = V.fromList eofSrc
      eofInp = new_input eofVec

  case parse_expr eofInp of
    Left err -> printParseError eofVec err
    Right (e, _) -> print e


  ------------------------------------------------------------
  putStrLn "\n== Done =="


