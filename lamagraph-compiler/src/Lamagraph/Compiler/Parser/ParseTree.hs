module Lamagraph.Compiler.Parser.ParseTree () where

import Relude

data Location = Location
  {
  }
  deriving (Eq, Show)

type Longident = NonEmpty Text

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

data CoreType = CoreType
  { _pTyp :: CoreTypeDesc
  , _pTypLoc :: Location
  }
  deriving (Eq, Show)

data CoreTypeDesc
  = PTypVar Text
  | PTypArrow CoreType CoreType
  | PTypTuple CoreType (NonEmpty CoreType) -- n >= 2
  | PTypConstr Longident [CoreType]
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
  | PPatTuple CoreType (NonEmpty CoreType) -- n >= 2
  | PPatConstruct Longident (Maybe Pattern) -- TODO: revise
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
  | PExpFunction Pattern Expression -- nesting!
  | PExpApply Expression (NonEmpty Expression)
  | PExpMatch Expression (NonEmpty Case)
  | PExpTuple Expression (NonEmpty Expression) -- n >=2
  | PExpConstruct Longident (Maybe Expression)
  | PExpIfThenElse Expression Expression Expression
  | PExpConstraint Expression CoreType
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
  , _pTypeManifest :: Maybe CoreType
  , _pTypeLoc :: Location
  }
  deriving (Eq, Show)

data TypeKind
  = PTypeAbstract
  | PTypeVariant (NonEmpty ConstructorDeclaration)
  deriving (Eq, Show)

data ConstructorDeclaration = ConstructorDeclaration
  { _pCDName :: Text
  , _pCDArgs :: [CoreType]
  , _pCDRes :: Maybe CoreType -- TODO: I have no clue :)
  , _pCDLoc :: Location
  }
  deriving (Eq, Show)

data ValueBinding = ValueBinding
  { _pVBPat :: Pattern
  , _pVBExpr :: Expression
  , _pVBConstraint :: Maybe CoreType
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

data StructureItem
  = PStrEval Expression
  | PStrValue RecFlag (NonEmpty ValueBinding)
  | PStrType RecFlag (NonEmpty TypeDeclaration)
  | PStrModule ModuleDefinition
  | PStrOpen OpenDeclaration
  deriving (Eq, Show)
