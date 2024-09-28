{-# LANGUAGE TemplateHaskell #-}

module Lamagraph.Compiler.Parser.LexerTypes (
  Location (..),
  startPos,
  endPos,
  IdentType (..),
  TokenType (..),
  _TokIdent,
  _TokInt,
  _TokInt32,
  _TokUInt32,
  _TokInt64,
  _TokUInt64,
  _TokChar,
  _TokString,
  _TokInfixSymbol,
  _TokPrefixSymbol,
  AlexUserState (..),
  alexInitUserState,
  lexerCommentDepth,
  lexerStringStartPos,
  lexerStringValue,
  lexerReadString,
  Token (..),
  tokenType,
  loc,
  readStr,
) where

import Relude

import Control.Lens

import {-# SOURCE #-} Lamagraph.Compiler.Parser.Lexer

data Location = Loc
  { _startPos :: AlexPosn
  , _endPos :: AlexPosn
  }
  deriving (Eq, Show)
makeLenses ''Location

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
  | {- Character literals -}
    TokChar Char
  | {- String literals -}
    TokString Text
  | {- Operators -}
    TokInfixSymbol Text
  | TokPrefixSymbol Text
  | {- Keywords-}
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
  | -- | @<@
    TokLess
  | -- | @>@
    TokGreater
  | TokEOF
  deriving (Eq, Show)
makePrisms ''TokenType

data Token = Token
  { _tokenType :: TokenType
  , _loc :: Location
  , _readStr :: Text
  }
  deriving (Eq, Show)
makeLenses ''Token
