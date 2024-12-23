{-# LANGUAGE TemplateHaskell #-}

-- | Module for types produced by lexer
module Lamagraph.Compiler.Parser.LexerTypes (
  IdentType (..),
  Token (..),
  LToken,
  AlexUserState (..),
  alexInitUserState,
  lexerCommentDepth,
  lexerStringStartPos,
  lexerStringValue,
) where

import Relude

import Control.Lens

import Lamagraph.Compiler.Parser.SrcLoc

data AlexUserState = AlexUserState
  { _lexerCommentDepth :: Int
  , _lexerStringStartPos :: Maybe SrcLoc
  -- ^ t'SrcLoc' here allows to untie cyclic dependency on 'Lamagraph.Compiler.Parser.Lexer.AlexPosn'
  , _lexerStringValue :: Text
  }
  deriving (Eq, Show)
makeLenses ''AlexUserState

alexInitUserState :: AlexUserState
alexInitUserState =
  AlexUserState
    { _lexerCommentDepth = 0
    , _lexerStringStartPos = Nothing
    , _lexerStringValue = ""
    }

data IdentType = Capitalized | Lowercase
  deriving (Eq, Show)

data Token
  = TokIdent IdentType Text
  | {- Integer literals -}
    TokInt Int
  | {- Character literals -}
    TokChar Char
  | {- String literals -}
    TokString Text
  | {- Operators -}

    -- | @( = | \< | \> | __|__ | \& | $ ) /operator-char/*@
    TokInfixSymbol0 Text
  | -- | @( \@ | \^ ) /operator-char/*@
    TokInfixSymbol1 Text
  | -- | @( + | - ) /operator-char/*@
    TokInfixSymbol2 Text
  | -- | @( * | \/ | % ) /operator-char/* | lor | lxor | mod | land@
    TokInfixSymbol3 Text
  | -- | @** /operator-char/* | lsl | lsr | asr@
    TokInfixSymbol4 Text
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
  | -- | @.@
    TokDot
  | -- | @|@
    TokBar
  | -- | @||@
    TokDoubleBar
  | TokEOF
  deriving (Eq, Show)

type LToken = Located Token
