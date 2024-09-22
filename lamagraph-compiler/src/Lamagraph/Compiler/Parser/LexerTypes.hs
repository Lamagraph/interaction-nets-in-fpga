{-# LANGUAGE TemplateHaskell #-}

module Lamagraph.Compiler.Parser.LexerTypes (
  IdentType (..),
  TokenType (..),
  AlexUserState (..),
  alexInitUserState,
  lexerCommentDepth,
  lexerStringStartPos,
  lexerStringValue,
  lexerReadString,
  Token (..),
  tokenType,
  startPos,
  endPos,
  readStr,
) where

import Relude

import Control.Lens

import {-# SOURCE #-} Lamagraph.Compiler.Parser.Lexer

data AlexUserState = AlexUserState
  { _lexerCommentDepth :: Int
  , _lexerStringStartPos :: Maybe AlexPosn
  , _lexerStringValue :: Text
  , _lexerReadString :: Text
  }
  deriving (Eq, Show)
makeLenses ''AlexUserState

alexInitUserState :: AlexUserState
alexInitUserState =
  AlexUserState
    { _lexerCommentDepth = 0
    , _lexerStringStartPos = Nothing
    , _lexerStringValue = ""
    , _lexerReadString = ""
    }

data IdentType = Capitalized | Lowercase
  deriving (Eq, Show)

data TokenType
  = TokIdent IdentType Text
  | TokInt Int
  | TokInt32 Int32
  | TokUInt32 Word32
  | TokInt64 Int64
  | TokUInt64 Word64
  | -- Character literals
    TokChar Char
  | -- String literals
    TokString Text
  | -- Operators
    TokInfixSymbol Text
  | TokPrefixSymbol Text
  | -- Keywords
    TokAnd
  | TokAsr
  | TokElse
  | TokFalse
  | TokFun
  | TokIf
  | TokIn
  | TokLand
  | TokLet
  | TokLor
  | TokLsl
  | TokLsr
  | TokLxor
  | TokMatch
  | TokMod
  | TokModule
  | TokOf
  | TokOpen
  | TokRec
  | TokThen
  | TokTrue
  | TokType
  | TokWhen
  | TokWith
  | -- | @&&@
    TokBoolAnd
  | -- | @'@
    TokApostrophe
  | -- | @(@
    TokLeftPar
  | -- | @)@
    TokRightPar
  | -- | @*@
    TokStar
  | -- | @+@
    TokPlus
  | -- | @,@
    TokComma
  | -- | @-@
    TokMinus
  | -- | @->@
    TokArrow
  | -- | @:@
    TokColon
  | -- | @::@
    TokDoubleColon
  | -- | @;@
    TokSemicolon
  | -- | @=@
    TokEq
  | -- | @[@
    TokLeftBracket
  | -- | @]@
    TokRightBracket
  | -- | @_@
    TokWildcard
  | -- | @{@
    TokLeftCurly
  | -- | @}@
    TokRightCurly
  | -- | @.@
    TokDot
  | -- | @|@
    TokBar
  | -- | @||@
    TokDoubleBar
  | TokEOF
  deriving (Eq, Show)

data Token = Token
  { _tokenType :: TokenType
  , _startPos :: AlexPosn
  , _endPos :: AlexPosn
  , _readStr :: Text
  }
  deriving (Eq, Show)
makeLenses ''Token
