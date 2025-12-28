{-# LANGUAGE PatternSynonyms #-}

module Types
  ( Pos(..)
  , Span(..)
  , Input(..)
  , Token(..)
  , AST(..)
  , ParseError(..)
  , Value(..)
  , Func(..)
  , EvalError(..)
  , Result
  , pattern Ok
  , pattern Err
  )
where


import Data.Text (Text)
import Data.Word (Word32, Word64)
import qualified Data.Vector()
import Data.Vector (Vector)
import Data.Type.Bool ()

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
  = TokenIdent  Text
  | TokenLambdaVar  Text
  | TokenNum    Word64
  | TokenString Text
  | TokenLParen
  | TokenRParen
  | TokenEqual
  | TokenPlus
  | TokenMinus
  | TokenStar
  | TokenSlash
  deriving (Show, Eq)

data AST
  = ASTNum    Span Word64
  | ASTString Span Text
  | ASTSymbol Span Text
  | ASTCall   Span [AST]
  | ASTAssign Span AST AST
  | ASTAdd    Span AST AST
  | ASTSub  Span AST AST
  | ASTMul    Span AST AST
  | ASTDiv    Span AST AST
  | ASTVoid   Span
  deriving (Show, Eq)

data Func = Func 
  deriving (Show,Eq)

data Value
  = ValNum    Word64
  | ValString Text
  | ValVoid
  | ValFunc Func
  deriving (Show,Eq)

data ParseError
  = UnexpectedEOF Pos
  | UnexpectedToken Span
  | ExpectedButGot Text Span
  | NotClosed Span Pos --start end
  deriving (Show, Eq)

data EvalError
  = NotAFunction Span
  | WrongNumberOfArguments Span Int Int -- expected got
  | UnknownVar Span Text
   deriving (Show,Eq)


type Result e a = Either e a

pattern Ok :: a -> Result e a
pattern Ok a = Right a

pattern Err :: e -> Result e a
pattern Err e = Left e

{-# COMPLETE Ok, Err #-}