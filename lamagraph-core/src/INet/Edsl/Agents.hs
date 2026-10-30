{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : INet.Edsl.Agents
Description : Template Haskell generator for an agent-set declaration skeleton

An agent set needs five declarations, all determined by the same
@(name, secondary arity)@ pairs:

* the flat runtime-label enum stored in @Node._nodeType@;
* the promoted-tag enum @AgentKind@;
* the kind-indexed secondary-arity type family @AgentArity@;
* the singleton GADT @Agent@, whose constructors are the argument of
  'INet.Edsl.fresh';
* one 'INet.Edsl.AgentLabel' instance per agent, bridging the singleton type
  to the associated arity family and to the runtime label.

Hand-writing them means repeating each tuple five times and letting the copies
drift; one splice from a single declaration list keeps that list the only
source of truth. Template Haskell is used /only/ for this mechanical skeleton:
rule bodies and the @INet@ instance stay hand-written because they carry the
actual reduction semantics.

Per-agent names are derived by suffix: the pair @(\"Apply\", 2)@ yields the
singleton constructor @Apply@, the promoted tag @ApplyKind@, and the runtime
label @ApplyLabel@. Only the name of the runtime label /type/ is an argument;
the kind, arity-family, and singleton type names are fixed to @AgentKind@,
@AgentArity@, and @Agent@.

Example (the lambda-calculus set from "INet.Define"):

@
\$('declareAgents' \"AgentSimpleLambda\"
  [ (\"Apply\", 2)
  , (\"Abstract\", 2)
  , (\"Erase\", 0)
  , (\"Duplicate\", 2)
  ])
@

The generated label and kind enums derive @Generic@, @NFDataX@, @Show@, @Eq@,
and @ShowX@ — the set Clash needs for anything stored in hardware state or
exposed on a port. The deriving strategy is left to GHC, so stock classes and
Clash's anyclass instances are picked as if the clauses were written by hand.
-}
module INet.Edsl.Agents (
  AgentDecl,
  declareAgents,
) where

import Clash.Prelude (NFDataX, ShowX)
import Data.List (nub)
import GHC.Generics (Generic)
import GHC.TypeNats (Nat)
import Language.Haskell.TH
import Prelude

{- | One agent: the singleton constructor name and its secondary arity.
The tag constructor is @<name>Kind@ and the runtime label @<name>Label@.
-}
type AgentDecl = (String, Int)

{- | Expand a non-empty agent list into the declarations described in the
module header: the runtime-label enum, @AgentKind@, @AgentArity@, @Agent@, and
one @AgentLabel@ instance per agent.

@declareAgents labelTypeName agents@ names the runtime label type and gives one
@(constructor name, secondary arity)@ pair per agent. A malformed declaration
fails the splice, i.e. becomes a compile error: an empty agent list, an empty
or duplicate constructor name, a negative arity, or an empty label-type name.
-}
declareAgents :: String -> [AgentDecl] -> Q [Dec]
declareAgents labelTypeName agents = do
  validate
  pure $
    labelDecl labelTypeName agents
      : kindDecl agents
      : familyDecl agents
      : singletonDecl agents
      : map (bridgeInstance labelTypeName) agents
 where
  validate
    | null agents = fail "declareAgents: no agents given"
    | null labelTypeName = fail "declareAgents: empty runtime label type name"
    | any ((< 0) . snd) agents = fail "declareAgents: negative secondary arity"
    | length (map fst agents) /= length (nub (map fst agents)) =
        fail "declareAgents: duplicate agent constructor name"
    | any (null . fst) agents = fail "declareAgents: empty agent constructor name"
    | otherwise = pure ()

-- Fixed type-level names of the generated skeleton.
kindName, familyName, singletonName :: String
kindName = "AgentKind"
familyName = "AgentArity"
singletonName = "Agent"

-- | @<labelType> = <Name>Label | ...@ with the Clash/Prelude deriving set.
labelDecl :: String -> [AgentDecl] -> Dec
labelDecl labelTypeName agents =
  DataD
    []
    (mkName labelTypeName)
    []
    Nothing
    [NormalC (mkName (labelCtor d)) [] | d <- agents]
    [derivingClause [''Generic, ''NFDataX, ''Show, ''Eq, ''ShowX]]

-- | @data AgentKind = <Name>Kind | ...@ (promoted-tag enum).
kindDecl :: [AgentDecl] -> Dec
kindDecl agents =
  DataD
    []
    (mkName kindName)
    []
    Nothing
    [NormalC (mkName (tagCtor d)) [] | d <- agents]
    [derivingClause [''Generic, ''Show, ''Eq, ''NFDataX, ''ShowX]]

-- | @type family AgentArity (k :: AgentKind) :: Nat where ...@.
familyDecl :: [AgentDecl] -> Dec
familyDecl agents =
  ClosedTypeFamilyD
    ( TypeFamilyHead
        (mkName familyName)
        [KindedTV (mkName "k") BndrReq (ConT (mkName kindName))]
        (KindSig (ConT ''Nat))
        Nothing
    )
    [ TySynEqn
        Nothing
        (AppT (ConT (mkName familyName)) (tagTy d))
        (LitT (NumTyLit (toInteger (snd d))))
    | d <- agents
    ]

-- | @data Agent (k :: AgentKind) where <Name> :: Agent '<Name>Kind; ...@.
singletonDecl :: [AgentDecl] -> Dec
singletonDecl agents =
  DataD
    []
    (mkName singletonName)
    [KindedTV (mkName "k") BndrReq (ConT (mkName kindName))]
    Nothing
    [ GadtC [mkName (fst d)] [] (singTy d)
    | d <- agents
    ]
    []

{- | @instance AgentLabel (Agent '<Name>Kind) <labelType>@ with the associated
arity equation forwarding to the kind-indexed family, and the label mapping.
-}
bridgeInstance :: String -> AgentDecl -> Dec
bridgeInstance labelTypeName d =
  InstanceD
    Nothing
    []
    ( AppT
        (AppT (ConT (mkName "AgentLabel")) (singTy d))
        (ConT (mkName labelTypeName))
    )
    [ TySynInstD
        ( TySynEqn
            Nothing
            (AppT (ConT (mkName "AgentArityOf")) (singTy d))
            (AppT (ConT (mkName familyName)) (tagTy d))
        )
    , ValD
        (VarP (mkName "agentLabel"))
        (NormalB (LamE [WildP] (ConE (mkName (labelCtor d)))))
        []
    ]

-- | @Agent '<Name>Kind@
singTy :: AgentDecl -> Type
singTy d = AppT (ConT (mkName singletonName)) (tagTy d)

-- | @'<Name>Kind@
tagTy :: AgentDecl -> Type
tagTy = PromotedT . mkName . tagCtor

-- | @<Name>Kind@
tagCtor :: AgentDecl -> String
tagCtor = (++ "Kind") . fst

-- | @<Name>Label@
labelCtor :: AgentDecl -> String
labelCtor = (++ "Label") . fst

{- | No explicit strategy: GHC picks stock for @Eq@/@Show@/@Generic@ and
anyclass for the Clash classes (@NFDataX@/@ShowX@), matching the hand-written
@deriving@ clauses this generator replaces.
-}
derivingClause :: [Name] -> DerivClause
derivingClause = DerivClause Nothing . map ConT
