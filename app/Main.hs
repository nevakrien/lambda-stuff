{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V

import           Data.Text (Text)
import           Data.Vector   (Vector)
import           Data.Word     (Word32)

read_file :: FilePath -> IO (Vector Text)
read_file path = do
    txt <- T.readFile path
    pure (V.fromList (T.lines txt))


data Pos = Pos
  { line :: !Word32
  , col  :: !Word32
  } deriving (Show, Eq, Ord)

data Span = Span
  { start :: !Pos
  , end   :: !Pos
  } deriving (Show, Eq, Ord)

print_span :: (Vector Text) -> Span -> IO ()
print_span lines span
  | start span > end span = pure ()  -- sanity guard
  | otherwise              = mapM_ printLine [sLine .. eLine]
  where
    s       = start span
    e       = end   span
    sLine   = fromIntegral (line s) :: Int
    eLine   = fromIntegral (line e) :: Int
    sCol    = fromIntegral (col  s) :: Int
    eCol    = fromIntegral (col  e) :: Int

    printLine :: Int -> IO ()
    printLine i = do
      let lineText = lines V.! i
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


    sliceLine :: Text -> Int -> Int -> Text
    sliceLine t a b
      | b <= a    = T.empty
      | otherwise = T.take (b - a) (T.drop a t)


main :: IO ()
main = do
    ls <- read_file "README.md"
    print_span ls (Span (Pos 0 2) (Pos 0 7))