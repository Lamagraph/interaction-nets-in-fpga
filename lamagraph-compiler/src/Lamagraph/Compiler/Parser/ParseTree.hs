{-# LANGUAGE TemplateHaskell #-}

module Lamagraph.Compiler.Parser.ParseTree (
  Longident,
  Constant (..),
  pConstDesc,
  pConstLoc,
  ConstantDesc (..),
  CoreType (..),
  pTyp,
  pTypLoc,
  CoreTypeDesc (..),
  Pattern (..),
  pPatDesc,
  pPatLoc,
  PatternDesc (..),
  RecFlag (..),
  Expression (..),
  pExpDesc,
  pExpLoc,
  ExpressionDesc (..),
  Case (..),
  pCLhs,
  pCGuard,
  pCRhs,
  TypeDeclaration (..),
  TypeKind (..),
  ConstructorDeclaration (..),
  ValueBinding (..),
  pVBPat,
  pVBExpr,
  pVBLoc,
  ModuleDefinition (..),
  OpenDeclaration (..),
  Structure,
  StructureItem (..),
  ModuleExpr (..),
) where

import Relude

import Control.Lens

import Lamagraph.Compiler.Parser.LexerTypes

type Longident = NonEmpty Text

{- | Representation of constant values

/Note/: in contrast to the grammar, general constructors
and specific ones like @false@, @true@, @()@, @::@
aren't represented by this type
-}
data Constant = Constant
  { _pConstDesc :: ConstantDesc
  , _pConstLoc :: Location
  }
  deriving (Eq, Show)

data ConstantDesc
  = PConstInt Int
  | PConstInt32 Int32
  | PConstUInt32 Word32
  | PConstInt64 Int64
  | PConstUInt64 Word64
  | PConstChar Char
  | PConstString Text
  deriving (Eq, Show)

{- |
Type for representing type expressions
-}
data CoreType = CoreType
  { _pTyp :: CoreTypeDesc
  , _pTypLoc :: Location
  }
  deriving (Eq, Show)

data CoreTypeDesc
  = -- | Represents type variables like @'a@
    PTypVar Text
  | -- | Represents type arrow @'a -> 'a@
    PTypArrow CoreType CoreType
  | -- | Represents tuple @('a1, 'a2, ..., 'an)@
    --
    -- /Invariant/: \(n \geq 2\)
    PTypTuple CoreType (NonEmpty CoreType)
  | -- | @'PTypConstr' lident types@ represents
    --
    -- - @/typeconstr/@ when @types = []@
    -- - @/typexpr/ /typeconstr/@ when @types = [type]@
    -- - @( /typexpr/ { , /typeexpr/} ) /typeconstr/@ when @types = [type1, ..., typen]@
    PTypConstr Longident [CoreType]
  deriving (Eq, Show)

data Pattern = Pattern
  { _pPatDesc :: PatternDesc
  , _pPatLoc :: Location
  }
  deriving (Eq, Show)

data PatternDesc
  = PPatAny
  | PPatVar Text
  | PPatConstant Constant
  | -- | n >= 2
    PPatTuple Pattern (NonEmpty Pattern)
  | PPatConstruct Longident (Maybe Pattern)
  | PPatOr Pattern Pattern
  | PPatConstraint Pattern CoreType
  deriving (Eq, Show)

data RecFlag = Recursive | NonRecursive
  deriving (Eq, Show)

data Expression = Expression
  { _pExpDesc :: ExpressionDesc
  , _pExpLoc :: Location
  }
  deriving (Eq, Show)

data ExpressionDesc
  = PExpIdent Longident
  | PExpConstant Constant
  | PExpLet RecFlag (NonEmpty ValueBinding) Expression
  | -- | nesting!
    PExpFunction Pattern Expression
  | PExpApply Expression (NonEmpty Expression)
  | PExpMatch Expression (NonEmpty Case)
  | -- | Invariant: \(n \geq 2\)
    PExpTuple Expression (NonEmpty Expression)
  | PExpConstruct Longident (Maybe Expression)
  | PExpIfThenElse Expression Expression Expression
  | -- | @(expr : typexpr)@
    PExpConstraint Expression CoreType
  deriving (Eq, Show)

data Case = Case
  { _pCLhs :: Pattern
  , _pCGuard :: Maybe Expression
  , _pCRhs :: Expression
  }
  deriving (Eq, Show)

data TypeDeclaration = TypeDeclaration
  { _pTypeName :: Text
  , _pTypeParams :: [CoreType]
  , _pTypeKind :: TypeKind
  , _pTypeLoc :: Location
  }
  deriving (Eq, Show)

data TypeKind
  = PTypeAbstract (Maybe CoreType)
  | PTypeVariant [ConstructorDeclaration]
  deriving (Eq, Show)

data ConstructorDeclaration = ConstructorDeclaration
  { _pCDName :: Text
  , _pCDArgs :: [CoreType]
  , _pCDLoc :: Location
  }
  deriving (Eq, Show)

data ValueBinding = ValueBinding
  { _pVBPat :: Pattern
  , _pVBExpr :: Expression
  , _pVBLoc :: Location
  }
  deriving (Eq, Show)

data ModuleDefinition = ModuleDefinition
  { _pMDName :: Longident
  , _pMDLoc :: Location
  }
  deriving (Eq, Show)

data OpenDeclaration = OpenDeclaration
  { _pODName :: Longident
  , _pODLoc :: Location
  }
  deriving (Eq, Show)

type Structure = [StructureItem]

data StructureItem -- FIXME: Location?
  = PStrValue RecFlag (NonEmpty ValueBinding)
  | PStrType (NonEmpty TypeDeclaration)
  | PStrOpen OpenDeclaration
  deriving (Eq, Show)

data ModuleExpr = ModuleExpr
  { _pMEDefinition :: Maybe ModuleDefinition
  , _pMEStructure :: Structure
  }
  deriving (Eq, Show)

makeLenses ''Constant
makeLenses ''CoreType
makeLenses ''Pattern
makeLenses ''Expression
makeLenses ''Case
makeLenses ''ValueBinding
