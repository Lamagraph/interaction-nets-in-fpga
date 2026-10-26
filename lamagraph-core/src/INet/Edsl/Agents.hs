{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : INet.Edsl.Agents
Description : Template Haskell generator for an agent-set declaration skeleton

One list of @(constructor, secondary arity)@ pairs expands into the whole
declaration block an agent set needs to run on the "INet.Edsl" machinery:

* the flat runtime label enum stored in @Node._nodeType@;
* the kind enum of promoted tags (@AgentKind@);
* the kind-indexed secondary-arity type family (@AgentArity@);
* the singleton GADT (@Agent@) used as the argument of 'INet.Edsl.fresh';
* one @AgentLabel@ instance per agent, bridging the singleton type to the
  associated 'INet.Edsl.AgentArityOf' family and to the runtime label.

The names are conventional: the pair @(\"Apply\", 2)@ generates the singleton
constructor @Apply@, the promoted tag @ApplyKind@, and the runtime label
@ApplyLabel@. Only the name of the runtime label /type/ is supplied by the
caller (e.g. @\"AgentSimpleLambda\"@); the kind, arity-family, and singleton
type names are fixed (@AgentKind@, @AgentArity@, @Agent@).

Example (the lambda-calculus set from "INet.Define"):

@
\$('declareAgents' \"AgentSimpleLambda\"
  [ (\"Apply\", 2)
  , (\"Abstract\", 2)
  , (\"Erase\", 0)
  , (\"Duplicate\", 2)
  ])
@
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

{- | Expand an agent set into its label enum, kind enum, arity family,
singleton GADT, and one @AgentLabel@ instance per agent.

@declareAgents labelTypeName agents@ — the first argument names the runtime
label enum; the agent list is non-empty and all arities are non-negative.
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
