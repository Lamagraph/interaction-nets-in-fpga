{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Relude

import Data.View
import GraphRewriting
import GraphRewriting.Lib.Lib
import GraphRewriting.Pattern.InteractionNet

import Term qualified as Term

-- The signature of the graph is determined by the node type we provide. For each node constructor we define as record fields a fixed collection of ports. Here we name ports attached at the top of the nodes input ports and nodes at the bottom output ports.
data SKI
  = R {out :: Port} -- Root node, which is supposed to occur exactly once in the graph and correpsonds to the root of the term
  | A {inp, out1, out2 :: Port} -- An applicator. It's left-hand subgraph (out1) denotes the expression to which the expression represented by the right-hand subgraph is applied.
  | I {inp :: Port} -- The identity combinator.
  | E {inp :: Port} -- An eraser. This node type is used to delete the subgraph discarded by the K combinator.
  | D {inp1, inp2, out :: Port} -- A duplicator is used to implement sharing in the SKI combinator calculus
  | V {inp :: Port, name :: String} -- A free variable
  -- Here we implement the SKI combinators in a very fine-grained manner, namely a combinator has to accumulate its arguments one-by-one before it can be applied. That is why we have two variants of the K combinator: K0 (no arguments accumulated) and K1 (saturated).
  | K0 {inp :: Port}
  | K1 {inp :: Port, out :: Port}
  | -- The same goes for the S combinator, which takes even one more parameter.
    S0 {inp :: Port}
  | S1 {inp :: Port, out :: Port}
  | S2 {inp :: Port, out1 :: Port, out2 :: Port}
  deriving (Show, Eq)

-- While it is very convenient to specify the nodes' ports as record fields as above it does not reveal the graph structure to the library. Therefore we have to provide some boilerplate code to expose the ports, for which we use the 'View' abstraction. In the future some Template Haskell might be included in the library to avoid this effort.
instance View [Port] SKI where
  -- For each node type we simply have to return a list containing all the ports.
  inspect ski = case ski of
    R{out = o} -> [o]
    A{inp = i, out1 = o1, out2 = o2} -> [i, o1, o2]
    I{inp = i} -> [i]
    E{inp = i} -> [i]
    D{inp1 = i1, inp2 = i2, out = o} -> [i1, i2, o]
    V{inp = i} -> [i]
    K0{inp = i} -> [i]
    K1{inp = i, out = o} -> [i, o]
    S0{inp = i} -> [i]
    S1{inp = i, out = o} -> [i, o]
    S2{inp = i, out1 = o1, out2 = o2} -> [i, o1, o2]

  -- But we also need to provide means for the library to update these ports.
  update ports ski = case ski of
    R{} -> ski{out = o} where [o] = ports
    A{} -> ski{inp = i, out1 = o1, out2 = o2} where [i, o1, o2] = ports
    I{} -> ski{inp = i} where [i] = ports
    E{} -> ski{inp = i} where [i] = ports
    D{} -> ski{inp1 = i1, inp2 = i2, out = o} where [i1, i2, o] = ports
    V{} -> ski{inp = i} where [i] = ports
    K0{} -> ski{inp = i} where [i] = ports
    K1{} -> ski{inp = i, out = o} where [i, o] = ports
    S0{} -> ski{inp = i} where [i] = ports
    S1{} -> ski{inp = i, out = o} where [i, o] = ports
    S2{} -> ski{inp = i, out1 = o1, out2 = o2} where [i, o1, o2] = ports

-- Since we want to make use of interaction net reductions (using the 'activePair' pattern) we need to specify the principal port for each node type in the form of an index into the port list above.
instance (View SKI n) => INet n where
  principalPort n = case inspect n of
    R{out = o} -> o
    A{inp = i, out1 = o1, out2 = o2} -> o1
    I{inp = i} -> i
    E{inp = i} -> i
    D{inp1 = i1, inp2 = i2, out = o} -> o
    V{inp = i} -> i
    K0{inp = i} -> i
    K1{inp = i, out = o} -> i
    S0{inp = i} -> i
    S1{inp = i, out = o} -> i
    S2{inp = i, out1 = o1, out2 = o2} -> i

-- In "Term" a little SKI term parser is given. The code below implements a small compiler that translates the abstract syntax tree into a graph. Here you can see how primitive graph transformation functions like 'newNode' and 'newEdge' can be used to build a graph inside the 'GraphRewriting.Graph.Rewrite' monad. Also it shows how an edge can be attached to a node's port, simply by assigning it to the corresponding record field.
fromTerm :: Term.Expr -> Graph SKI
fromTerm term = flip execGraph emptyGraph $ do
  e <- compile term
  newNode R{out = e}

compile :: Term.Expr -> Rewrite SKI Edge
compile term = do
  e <- newEdge
  void $ case term of
    Term.A f x -> do
      ef <- compile f
      ex <- compile x
      newNode A{inp = e, out1 = ef, out2 = ex}
    Term.S -> newNode S0{inp = e}
    Term.K -> newNode K0{inp = e}
    Term.I -> newNode I{inp = e}
    Term.V v -> newNode V{inp = e, name = v}
  return e

-- The simplest of the SKI combinators is the I combinator. We match on the active pair of an I node and an applicator using the 'activePair' function which resides in the 'Pattern' monad. Note that the :-: is an infix constructor and is just an alternative representation of a pair. With the left-hand side of the rule given, we build a rule out of it that erases the matched nodes and connects the edges at the input port and the right output port of the applicator using the 'rewire' function.
ruleI :: (View [Port] n, View SKI n) => Rule n
ruleI = do
  I{} :-: A{inp = iA, out2 = o2} <- activePair
  rewire [[iA, o2]]

-- The K0 node represents a K combinator that has not yet accumulated an argument, which is what this rule does. Again, we match an active pair of a K0 node and an applicator. Then we replace these nodes by a K1 node that has the right-hand subgraph of the applicator as an argument (at its output port).
ruleK0 :: (View [Port] n, View SKI n) => Rule n
ruleK0 = do
  K0{} :-: A{inp = iA, out2 = o2} <- activePair
  replace $ byNode K1{inp = iA, out = o2}

-- The 'replace*' functions can be used replace the matched nodes by a combination of new nodes and rewirings, hence the constructors 'Wire' and 'Node'.
ruleK1 :: (View [Port] n, View SKI n) => Rule n
ruleK1 = do
  K1{out = oK} :-: A{inp = iA, out2 = o2A} <- activePair
  replace $ byWire iA oK <> byNode E{inp = o2A}

ruleS0 :: (View [Port] n, View SKI n) => Rule n
ruleS0 = do
  S0{} :-: A{inp = iA, out2 = o2A} <- activePair
  replace $ byNode S1{inp = iA, out = o2A}

ruleS1 :: (View [Port] n, View SKI n) => Rule n
ruleS1 = do
  S1{out = oS} :-: A{inp = iA, out2 = o2A} <- activePair
  replace $ byNode S2{inp = iA, out1 = oS, out2 = o2A}

-- If we need new edges for the right-hand side of the rewrite rule you can use 'replaceN' with N > 0.
ruleS2 :: (View [Port] n, View SKI n) => Rule n
ruleS2 = do
  S2{inp = iS, out1 = oS1, out2 = o2S} :-: a@A{out1 = o1A, out2 = o2A} <- activePair
  replace $ do
    (i1D, iB, i2D) <- (,,) <$> byEdge <*> byEdge <*> byEdge
    byNode A{inp = iS, out1 = oS1, out2 = i1D}
    byNode a{out2 = iB}
    byNode D{inp1 = i1D, inp2 = i2D, out = o2A}
    byNode A{inp = iB, out1 = o2S, out2 = i2D}

-- This is an abstraction to match any active pair that involves a node with arity 0.
arity0 :: (View [Port] n, View SKI n) => Pattern n (Pair SKI)
arity0 = i <|> k <|> s
 where
  i = do pair@(n :-: I{}) <- activePair; return pair
  k = do pair@(n :-: K0{}) <- activePair; return pair
  s = do pair@(n :-: S0{}) <- activePair; return pair

arity1 :: (View [Port] n, View SKI n) => Pattern n (Pair SKI)
arity1 = k <|> s
 where
  k = do pair@(n :-: K1{}) <- activePair; return pair
  s = do pair@(n :-: S1{}) <- activePair; return pair

-- If the left-hand side is to be erased completely without any rewirings or new nodes to be replaced with, use the 'erase'.
ruleE0 :: (View [Port] n, View SKI n) => Rule n
ruleE0 = do
  E{inp = iE} :-: n <- arity0
  erase

ruleE1 :: (View [Port] n, View SKI n) => Rule n
ruleE1 = do
  E{} :-: n <- arity1
  replace $ byNode E{inp = out n}

ruleE2 :: (View [Port] n, View SKI n) => Rule n
ruleE2 = do
  E{inp = iE} :-: S2{inp = iS, out1 = o1, out2 = o2} <- activePair
  replace $ byNode E{inp = o1} <> byNode E{inp = o2}

ruleD0 :: (View [Port] n, View SKI n) => Rule n
ruleD0 = do
  D{inp1 = iD1, inp2 = iD2, out = oD} :-: n <- arity0
  replace $ byNode n{inp = iD1} <> byNode n{inp = iD2}

ruleD1 :: (View [Port] n, View SKI n) => Rule n
ruleD1 = do
  D{inp1 = iD1, inp2 = iD2, out = oD} :-: n <- arity1
  replace $ do
    (iD1', iD2') <- (,) <$> byEdge <*> byEdge
    byNode n{inp = iD1, out = iD1'}
    byNode n{inp = iD2, out = iD2'}
    byNode D{inp1 = iD1', inp2 = iD2', out = out n}

ruleD2 :: (View [Port] n, View SKI n) => Rule n
ruleD2 = do
  D{inp1 = iD1, inp2 = iD2, out = oD} :-: S2{inp = iS, out1 = o1, out2 = o2} <- activePair
  replace $ do
    (l1, l2, x1, x2) <- (,,,) <$> byEdge <*> byEdge <*> byEdge <*> byEdge
    byNode S2{inp = iD1, out1 = l1, out2 = x1}
    byNode S2{inp = iD2, out1 = x2, out2 = l2}
    byNode D{inp1 = l1, inp2 = x2, out = o1}
    byNode D{inp1 = x1, inp2 = l2, out = o2}

-- Here is the only rule that is not an interaction-net reduction, hence it does not rely on the 'activePair' pattern. First we match on an eraser node anywhere in the graph. Next we require a duplicator node that is connected to the eraser. Therefore we use the 'previous' pattern that returns a reference to the previously matched node and feed it to the 'neighbour' function that matches on nodes connected to the referenced node.
eliminate :: (View [Port] n, View SKI n) => Rule n
eliminate = do
  E{inp = iE} <- node
  D{out = oD, inp1 = i1, inp2 = i2} <- nodeWith iE
  require (iE == i1 || iE == i2)
  if iE == i1
    then rewire [[oD, i2]]
    else rewire [[oD, i1]]

ruleTreeL =
  Branch
    "All"
    [ Leaf "Eliminate" eliminate
    , Branch "Erase" [Leaf "E0" ruleE0, Leaf "E1" ruleE1, Leaf "E2" ruleE2]
    , Branch "S" [Leaf "S0" ruleS0, Leaf "S1" ruleS1, Leaf "S2" ruleS2]
    , Branch "K" [Leaf "K0" ruleK0, Leaf "K1" ruleK1]
    , Leaf "I" ruleI
    , Branch "D" [Leaf "D0" ruleD0, Leaf "D1" ruleD1, Leaf "D2" ruleD2]
    ]

main :: IO ()
main = do
  let graph = fromTerm Term.skk
  putStrLn $ "Starting graph is:\n" ++ show graph
  resultGraph <- run 100 id pure graph ruleTreeL
  putStrLn $ "Result graph is:\n" ++ show resultGraph
  pure ()
