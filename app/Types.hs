module Types where

import Data.Text (Text)
import Data.Word (Word32, Word64)
import qualified Data.Vector as V
import Data.Vector (Vector)

data Pos = Pos
  { line :: !Word32
  , col  :: !Word32
  } deriving (Show, Eq, Ord)

data Span = Span
  { start :: !Pos
  , end   :: !Pos
  } deriving (Show, Eq, Ord)

data Input = Input {
    pos :: !Pos ,
    cur :: !Text ,
    rest :: !(Vector Text)
} deriving (Eq,Show)

data Token
  = TokIdent  Text
  | TokNum    Word64
  | TokString Text
  | TokLParen
  | TokRParen
  deriving (Show, Eq)

data Expr
  = ENum    Span Word64
  | EString Span Text
  | ESymbol Span Text
  | EList   Span [Expr]
  deriving (Show, Eq)

data ParseError
  = UnexpectedEOF Pos
  | UnexpectedToken Span
  | ExpectedButGot Text Span
  | NotCLose Span Pos --start end
  deriving (Show, Eq)