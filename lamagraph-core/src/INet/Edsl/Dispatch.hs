{- |
Module      : INet.Edsl.Dispatch
Description : Template Haskell generator for an EDSL rule table and its @INet@ instance

The rule table of an EDSL agent set is mechanical once the agent declarations
and the rule bodies exist: every entry is a pair of runtime labels plus a
`SomeRule`, and the `INet` methods are a fixed pattern-match over that lookup.
This module keeps only the semantic parts in source: the user writes the rule
bodies and one declarative @(left, right, rule)@ list; 'declareINet' expands
the list into

* @canonicalRule@ — the unordered table, one branch per entry;
* @ruleFor@ — canonical lookup plus the mirrored lookup, so reversed pairs
  need no extra entry;
* the whole @instance INet label nodes edges ports@, with
  @getAddressesAllocationCount@ reading the rule's type-level node count and
  @makeReduction@ delegating to 'runRule'.

The generated code is the same code that used to be written by hand in
"INet.Define", so it introduces no new runtime constructs for Clash.

== Usage

@
\$(declareAgents \"AgentSimpleLambda\" [...])

-- rule bodies and rule values
betaRule = ...

\$(declareINet
    \"AgentSimpleLambda\"
    4 2 2
    [ rule \"Apply\" \"Abstract\" [| betaRule |]
    , ...
    ])
@

The list is the only table the user writes. @nodesNumber@, @edgesNumber@ and
@portsNumber@ are the @INet@ instance parameters, in the class order
(@INet label nodesNumber edgesNumber portsNumber@); the generator maps them to
the @SomeRule edgesNumber nodesNumber portsNumber@ order internally.

Each unordered pair may appear at most once: 'ruleFor' derives the reversed
orientation by mirroring. A missing pair stays a runtime @errorX@, as before.
-}
module INet.Edsl.Dispatch (
  RuleDecl,
  rule,
  declareINet,
) where

import Clash.Prelude (errorX)
import Core.Node (LoadedNode (..), Node (..))
import Data.List (nub)
import INet.Edsl (SomeRule (..), mirrorSomeRule, ruleAllocationCount, runRule)
import INet.Net (INet (..))
import Language.Haskell.TH
import Prelude

{- | One rule-table entry: the left runtime label name, the right runtime label
name (without the @Label@ suffix), and the quoted rule value.
-}
type RuleDecl = (String, String, Q Exp)

-- | Smart constructor for a 'RuleDecl'.
rule :: String -> String -> Q Exp -> RuleDecl
rule = (,,)

{- | Expand a declarative rule table into @canonicalRule@, @ruleFor@, and the
@INet@ instance for the given label type.

@declareINet labelType nodesNumber edgesNumber portsNumber rules@ takes the
runtime label type name (the argument of 'INet.Edsl.Agents.declareAgents'),
the three @INet@ ceilings, and the table entries. It fails the splice on an
empty table, an empty label type name, negative ceilings, or duplicate
unordered pairs.
-}
declareINet ::
  String ->
  Integer ->
  Integer ->
  Integer ->
  [RuleDecl] ->
  Q [Dec]
declareINet labelTypeName nodesNumber edgesNumber portsNumber ruleDecls = do
  validate
  rules <- traverse (\(left, right, ruleExp) -> (,,) left right <$> ruleExp) ruleDecls
  pure (canonicalRuleDecl rules ++ ruleForDecl ++ instanceDecl)
 where
  validate
    | null labelTypeName = fail "declareINet: empty runtime label type name"
    | null ruleDecls = fail "declareINet: no rule declarations given"
    | nodesNumber < 0 || edgesNumber < 0 || portsNumber < 0 =
        fail "declareINet: node, edge and port ceilings must be non-negative"
    | not (null duplicatePairs) =
        fail $ "declareINet: duplicate rule pair(s): " ++ show duplicatePairs
    | otherwise = pure ()

  duplicatePairs =
    [ key
    | key <- nub (map pairKey ruleDecls)
    , length (filter (== key) (map pairKey ruleDecls)) > 1
    ]
  pairKey (left, right, _) =
    if left <= right then (left, right) else (right, left)

  leftName = mkName "left"
  rightName = mkName "right"
  ruleName = mkName "matchedRule"
  freeAddrsName = mkName "freeAddrs"
  leftNodeName = mkName "leftNode"
  rightNodeName = mkName "rightNode"
  canonicalName = mkName "canonicalRule"
  ruleForName = mkName "ruleFor"

  labelTy = ConT (mkName labelTypeName)
  natTy n = LitT (NumTyLit n)
  someRuleTy =
    foldl
      AppT
      (ConT ''SomeRule)
      [natTy edgesNumber, natTy nodesNumber, natTy portsNumber, labelTy]
  maybeSomeRuleTy = AppT (ConT ''Maybe) someRuleTy
  funTy argument = AppT (AppT ArrowT argument)
  ruleForTy = funTy labelTy (funTy labelTy maybeSomeRuleTy)

  labelCon name = ConP (mkName (name ++ "Label")) [] []
  canonicalAlt (left, right, ruleExp) =
    Match
      (TupP [labelCon left, labelCon right])
      (NormalB (AppE (ConE 'Just) (AppE (ConE 'SomeRule) ruleExp)))
      []
  noRuleAlt = Match WildP (NormalB (ConE 'Nothing)) []

  canonicalRuleDecl rules =
    [ SigD canonicalName ruleForTy
    , FunD
        canonicalName
        [ Clause
            [VarP leftName, VarP rightName]
            ( NormalB $
                CaseE
                  (TupE [Just (VarE leftName), Just (VarE rightName)])
                  (map canonicalAlt rules ++ [noRuleAlt])
            )
            []
        ]
    ]

  canonicalCall first = AppE (AppE (VarE canonicalName) first)

  ruleForDecl =
    [ SigD ruleForName ruleForTy
    , FunD
        ruleForName
        [ Clause
            [VarP leftName, VarP rightName]
            ( NormalB $
                CaseE
                  (canonicalCall (VarE leftName) (VarE rightName))
                  [ Match
                      (ConP 'Just [] [VarP ruleName])
                      (NormalB (AppE (ConE 'Just) (VarE ruleName)))
                      []
                  , Match
                      (ConP 'Nothing [] [])
                      ( NormalB $
                          AppE
                            (AppE (VarE 'fmap) (VarE 'mirrorSomeRule))
                            (canonicalCall (VarE rightName) (VarE leftName))
                      )
                      []
                  ]
            )
            []
        ]
    ]

  ruleForCall first = AppE (AppE (VarE ruleForName) first)

  allocationBody =
    CaseE
      (ruleForCall (VarE leftName) (VarE rightName))
      [ Match
          (ConP 'Just [] [ConP 'SomeRule [] [VarP ruleName]])
          (NormalB (AppE (VarE 'ruleAllocationCount) (VarE ruleName)))
          []
      , Match
          (ConP 'Nothing [] [])
          (NormalB (AppE (VarE 'errorX) (LitE (StringL "No rule for this pair"))))
          []
      ]

  getAddressesAllocationCountDecl =
    FunD
      'getAddressesAllocationCount
      [Clause [VarP leftName, VarP rightName] (NormalB allocationBody) []]

  nodeLabel expression =
    AppE (VarE '_nodeType) (AppE (VarE '_containedNode) expression)

  makeReductionBody =
    CaseE
      ( ruleForCall
          (nodeLabel (VarE leftNodeName))
          (nodeLabel (VarE rightNodeName))
      )
      [ Match
          (ConP 'Just [] [ConP 'SomeRule [] [VarP ruleName]])
          ( NormalB $
              AppE
                (AppE (AppE (AppE (VarE 'runRule) (VarE freeAddrsName)) (VarE ruleName)) (VarE leftNodeName))
                (VarE rightNodeName)
          )
          []
      , Match
          (ConP 'Nothing [] [])
          (NormalB (AppE (VarE 'errorX) (LitE (StringL "No rule for this pair"))))
          []
      ]

  makeReductionDecl =
    FunD
      'makeReduction
      [ Clause
          [VarP freeAddrsName, VarP leftNodeName, VarP rightNodeName]
          (NormalB makeReductionBody)
          []
      ]

  instanceTy =
    foldl
      AppT
      (ConT ''INet)
      [labelTy, natTy nodesNumber, natTy edgesNumber, natTy portsNumber]
  instanceDecl =
    [ InstanceD
        Nothing
        []
        instanceTy
        [getAddressesAllocationCountDecl, makeReductionDecl]
    ]
