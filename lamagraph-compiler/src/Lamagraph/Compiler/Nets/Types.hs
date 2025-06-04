{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

{- | Module with types for encoding of  Interaction Nets and their abstract machine.

Types are based on

* [1] M. Fernández and I. Mackie, “A Calculus for Interaction Nets,” in Principles and Practice of Declarative Programming, vol. 1702, G. Nadathur, Ed., in Lecture Notes in Computer Science, vol. 1702. , Berlin, Heidelberg: Springer Berlin Heidelberg, 1999, pp. 170–187. doi: 10.1007/10704567_10.
* [2] J. S. Pinto, “Sequential and Concurrent Abstract Machines for Interaction Nets,” in Foundations of Software Science and Computation Structures, J. Tiuryn, Ed., Berlin, Heidelberg: Springer, 2000, pp. 267–282. doi: 10.1007/3-540-46432-8_18.
-}
module Lamagraph.Compiler.Nets.Types (
  -- * Basic types
  -- $basic
  Var,
  Term (..),

  -- * Net
  -- $net
  Net (..),

  -- * Abstract machine
  -- $amachine
  AnnTerm (..),
  ThreadState (..),
  Configuration (..),
  Rule,

  -- * Helper types
  INsMachine,
  runINsMachine,
  INsException (..),
  HasReductionCounter (..),
  HasPreviousReductionCounter (..),
  HasReductionWidthHistory (..),
  INsMachineStats (..),
) where

import Relude hiding (newTVarIO, readTVarIO)

import Prettyprinter
import UnliftIO

import Lamagraph.Compiler.MonadFresh

{- $basic
These are basic types for building Interaction Nets.
Since Interaction Nets are graphs, not trees, we need variables to connect trees of agents.
Note that active pairs must be represented using another data structure like list of pairs.
-}
type Var = Int

data Term label
  = Var Var
  | Agent
      -- | Label for a given agent
      label
      -- | Auxiliary ports--other trees of agents
      [Term label]
  deriving (Show, Eq)

{- $net
Net representation from [1].
-}
data Net label = Net
  { terms :: ![Term label]
  -- ^ Interface of the net
  , equations :: ![(Term label, Term label)]
  -- ^ Active pairs and renamings of the net
  }
  deriving (Show)

{- $amachine
Types for a sequential abstract machine from [2].
-}

{- | Term annotated with list of variables inside and Nothing (white square in the paper)

Invariant: first argument must be empty list if Term is Var
-}
data AnnTerm label = AnnTerm [Maybe Var] (Term label) deriving (Show, Eq)

data ThreadState label where
  Process :: (AnnTerm label, AnnTerm label) -> ThreadState label
  Enlist :: [(AnnTerm label, AnnTerm label)] -> ThreadState label
  Delist :: ThreadState label
  Cycle :: (Var, AnnTerm label) -> ThreadState label
  deriving (Show, Eq)

data Configuration label = Configuration
  { heap :: !(Map Var (AnnTerm label))
  , stack :: !(NonEmpty (Maybe (AnnTerm label, AnnTerm label)))
  {- ^ Invariant: there must be Nothing at the end of the stack.
  This is required to properly track parallel execution statistics.
  Consider using 'mkConfigurationWithDefault'.
  -}
  , phi :: !(Map Var Var)
  -- ^ Invariant on map: for any pair (x, y) there must be a pair (y, x)
  , iface :: ![AnnTerm label]
  , cycles :: !(Map Var (AnnTerm label))
  , threadState :: !(ThreadState label)
  }
  deriving (Show)

type Rule label =
  -- | Label of the left agent
  label ->
  -- | Auxiliary ports of the left agent
  [Term label] ->
  -- | Label of the right agent
  label ->
  -- | Auxiliary ports of the right agent
  [Term label] ->
  -- | List of pairs for a stack (I_S in a paper) and new involution pairs (Phi_S in the paper)
  INsMachine ([(AnnTerm label, AnnTerm label)], Map Var Var)

data INsEnv = INsEnv
  { freshCounter :: !(TVar Var)
  , reductionCounter :: !(TVar Word64)
  , previousReductionCounter :: !(TVar Word64)
  , reductionWidthHistory :: !(TVar [Word64])
  }

instance HasFreshCounter INsEnv where
  getFreshCounter = freshCounter

class HasReductionCounter a where
  getReductionCounter :: a -> TVar Word64

instance HasReductionCounter INsEnv where
  getReductionCounter INsEnv{reductionCounter} = reductionCounter

class HasPreviousReductionCounter a where
  getPreviousReductionCounter :: a -> TVar Word64

instance HasPreviousReductionCounter INsEnv where
  getPreviousReductionCounter = previousReductionCounter

class HasReductionWidthHistory a where
  getReductionWidthHistory :: a -> TVar [Word64]

instance HasReductionWidthHistory INsEnv where
  getReductionWidthHistory INsEnv{reductionWidthHistory} = reductionWidthHistory

type INsMachine a = ReaderT INsEnv IO a

data INsMachineStats = INsMachineStats {reductionCounter :: Word64, reductionWidthHistory :: [Word64]} deriving (Show)

runINsMachine :: INsMachine a -> IO (a, INsMachineStats)
runINsMachine f = do
  freshCounter <- newTVarIO 0
  reductionCounter <- newTVarIO 0
  previousReductionCounter <- newTVarIO 0
  reductionWidthHistory <- newTVarIO []
  res <- runReaderT f INsEnv{..}
  reductionCounterRes <- readTVarIO previousReductionCounter
  reductionWidthHistoryRes <- readTVarIO reductionWidthHistory
  pure (res, INsMachineStats reductionCounterRes reductionWidthHistoryRes)

data INsException label
  = CannotApplyAnyRuleException (Configuration label)
  | DelistFromEmptyStackException (Configuration label)
  | DegenerateConfigurationException (Configuration label)
  | IncorrectNet (Net label) [(Var, Int)]
  deriving (Show, Typeable)
instance (Show label, Typeable label) => Exception (INsException label)

instance (Pretty label) => Pretty (Term label) where
  pretty = \case
    Var v -> pretty v
    Agent label aux -> pretty label <> if null aux then "" else parens $ fillSep $ punctuate comma (map pretty aux)

instance (Pretty label) => Pretty (Net label) where
  pretty Net{terms, equations} = angles $ fillSep (punctuate comma (map pretty terms)) <+> "|" <+> prettyEqs
   where
    prettyEqs = fillSep $ punctuate comma $ map (\(l, r) -> pretty l <+> "⋈" <+> pretty r) equations
