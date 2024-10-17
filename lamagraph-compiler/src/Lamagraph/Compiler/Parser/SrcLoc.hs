{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{- | This module contain types that describe positions of text in source files.
This types are used instead of 'Lamagraph.Compiler.Parser.Lexer.AlexPosn' outside of the internals of the lexer.

Inspired by GHC's SrcLoc.hs.
Thus, if some function is needed for compiler, please, consult with GHC first.
-}
module Lamagraph.Compiler.Parser.SrcLoc (
  -- * SrcLoc
  RealSrcLoc, -- abstract
  SrcLoc (..),

  -- ** Constructing SrcLoc
  mkSrcLoc,
  mkRealSrcLoc,
  generatedSrcLoc,
  mkBadSrcLoc,

  -- ** Accessing RealSrcLoc
  _RealSrcLoc',

  -- * SrcSpan
  RealSrcSpan, -- abstract
  SrcSpan (..),
  UnhelpfulSpanReason (..),

  -- ** Constructing SrcSpan
  mkBadSrcSpan,
  mkSrcSpan,
  mkRealSrcSpan,
  generatedSrcSpan,
  combineSrcSpans,

  -- ** Accessing RealSrcSpan
  srcSpanFile,
  srcSpanSLine,
  srcSpanSColumn,
  srcSpanELine,
  srcSpanEColumn,

  -- * Located
  Located,
  RealLocated,
  GenLocated (..),

  -- ** Operations on Located
  mkBadLocated,
  combineLocs,

  -- ** Accessing Located
  _L,
  unLoc,
  getLoc,

  -- * Parser locations
  combineRealSrcSpans,
) where

import Relude

import Control.Lens

{- | Real Source Location

Represents a single point in a file
-}
data RealSrcLoc
  = RealSrcLoc'
      Text -- Filename
      !Int -- Line number, starts from 1
      !Int -- Column number, starts from 1
  deriving (Eq, Show)

{- | Example access
 x ^. _RealSrcLoc' . _1
-}
makePrisms ''RealSrcLoc

-- | Source Location
data SrcLoc
  = RealSrcLoc !RealSrcLoc
  | UnhelpfulLoc Text -- With a reason why
  deriving (Show, Eq)

-- makePrisms ''SrcLoc

mkSrcLoc :: Text -> Int -> Int -> SrcLoc
mkSrcLoc file line column = RealSrcLoc (mkRealSrcLoc file line column)

mkRealSrcLoc :: Text -> Int -> Int -> RealSrcLoc
mkRealSrcLoc = RealSrcLoc'

generatedSrcLoc :: SrcLoc
generatedSrcLoc = UnhelpfulLoc "<compiler-generated code>"

mkBadSrcLoc :: Text -> SrcLoc
mkBadSrcLoc = UnhelpfulLoc

{- | Real Source Span

Represents a span in a source file using a pair of (line, column) coordinates
-}
data RealSrcSpan
  = RealSrcSpan'
  { _srcSpanFile :: Text
  , _srcSpanSLine :: !Int
  , _srcSpanSColumn :: !Int
  , _srcSpanELine :: !Int
  , _srcSpanEColumn :: !Int
  }
  deriving (Eq, Show)

makeLenses ''RealSrcSpan

{- | Source Span

A 'SrcSpan' represents either "good" portion of a file
or a description of a "bad" span.
-}
data SrcSpan = RealSrcSpan RealSrcSpan | UnhelpfulSpan UnhelpfulSpanReason deriving (Show, Eq)

data UnhelpfulSpanReason = UnhelpfulGenerated | UnhelpfulOther Text deriving (Show, Eq)

generatedSrcSpan :: SrcSpan
generatedSrcSpan = UnhelpfulSpan UnhelpfulGenerated

mkBadSrcSpan :: Text -> SrcSpan
mkBadSrcSpan = UnhelpfulSpan . UnhelpfulOther

mkRealSrcSpan :: RealSrcLoc -> RealSrcLoc -> RealSrcSpan
mkRealSrcSpan loc1 loc2 = RealSrcSpan'{..}
 where
  _srcSpanFile = loc1 ^. _RealSrcLoc' . _1
  _srcSpanSLine = loc1 ^. _RealSrcLoc' . _2
  _srcSpanSColumn = loc1 ^. _RealSrcLoc' . _3
  _srcSpanELine = loc2 ^. _RealSrcLoc' . _2
  _srcSpanEColumn = loc2 ^. _RealSrcLoc' . _3

-- | Create 'SrcSpan' between two positions in a file
mkSrcSpan :: SrcLoc -> SrcLoc -> SrcSpan
mkSrcSpan (UnhelpfulLoc str) _ = mkBadSrcSpan str
mkSrcSpan _ (UnhelpfulLoc str) = mkBadSrcSpan str
mkSrcSpan (RealSrcLoc loc1) (RealSrcLoc loc2) = RealSrcSpan $ mkRealSrcSpan loc1 loc2

{- | Combines two 'RealSrcSpan's into one that covers both original one.
Assumes file part is the same in both spans.
-}
combineRealSrcSpans :: RealSrcSpan -> RealSrcSpan -> RealSrcSpan
combineRealSrcSpans span1 span2 = RealSrcSpan'{..}
 where
  (_srcSpanSLine, _srcSpanSColumn) =
    min
      (span1 ^. srcSpanSLine, span1 ^. srcSpanSColumn)
      (span2 ^. srcSpanSLine, span2 ^. srcSpanSColumn)
  (_srcSpanELine, _srcSpanEColumn) =
    max
      (span1 ^. srcSpanELine, span1 ^. srcSpanEColumn)
      (span2 ^. srcSpanELine, span2 ^. srcSpanEColumn)
  _srcSpanFile = span1 ^. srcSpanFile

{- | Combines two 'SrcSpan's into one that covers both original one.
Returns 'UnhelpfulSpan' if files differ.
-}
combineSrcSpans :: SrcSpan -> SrcSpan -> SrcSpan
combineSrcSpans (UnhelpfulSpan _) r = r
combineSrcSpans l (UnhelpfulSpan _) = l
combineSrcSpans (RealSrcSpan span1) (RealSrcSpan span2)
  | span1 ^. srcSpanFile == span2 ^. srcSpanFile = RealSrcSpan $ combineRealSrcSpans span1 span2
  | otherwise = mkBadSrcSpan "<combineSrcSpans: files differ>"

-- | 'SrcSpan's will be attached to nearly everything, let's create a type for attaching.
data GenLocated l e = L l e deriving (Show)

{- | This is very specific instance for 'Eq'.
It doesn't compare locations, only contents.
-}
instance (Eq e) => Eq (GenLocated l e) where
  (==) :: GenLocated l e -> GenLocated l e -> Bool
  (L _ a) == (L _ b) = a == b

makePrisms ''GenLocated

type Located = GenLocated SrcSpan
type RealLocated = GenLocated RealSrcSpan

mkBadLocated :: Text -> e -> Located e
mkBadLocated str = L (mkBadSrcSpan str)

combineLocs :: Located a -> Located b -> SrcSpan
combineLocs (L loc1 _) (L loc2 _) = combineSrcSpans loc1 loc2

unLoc :: GenLocated l e -> e
unLoc (L _ e) = e

getLoc :: GenLocated l e -> l
getLoc (L loc _) = loc
