module Lamagraph.Compiler.Parser.SrcLocTest (srcLocTests) where

import Relude hiding (show)

import Control.Lens
import Test.Tasty
import Test.Tasty.HUnit
import Prelude (show)

import Lamagraph.Compiler.Parser.SrcLoc

-- | A real source location and two more, used throughout the tests.
locA :: RealSrcLoc
locA = mkRealSrcLoc "f.lml" 1 5

locB :: RealSrcLoc
locB = mkRealSrcLoc "f.lml" 3 9

locOtherFile :: RealSrcLoc
locOtherFile = mkRealSrcLoc "g.lml" 10 12

spanAB :: RealSrcSpan
spanAB = mkRealSrcSpan locA locB

srcLocTests :: TestTree
srcLocTests =
  testGroup
    "SrcLoc"
    [ testGroup
        "RealSrcLoc"
        [ testCase "construct + prism fields" $ do
            assertEqual "file" ("f.lml" :: Text) (locA ^. _RealSrcLoc' . _1)
            assertEqual "line" (1 :: Int) (locA ^. _RealSrcLoc' . _2)
            assertEqual "column" (5 :: Int) (locA ^. _RealSrcLoc' . _3)
        , testCase "Eq" $ do
            assertEqual "==" (mkRealSrcLoc "f.lml" 1 5) locA
            assertBool "/=" (locA /= locB)
        , testCase "Show" $ assertNonEmptyShow locA
        ]
    , testGroup
        "SrcLoc"
        [ testCase "mkSrcLoc" $ assertEqual "RealSrcLoc" (RealSrcLoc locA) (mkSrcLoc "f.lml" 1 5)
        , testCase "generatedSrcLoc" $ assertEqual "UnhelpfulLoc" (UnhelpfulLoc "<compiler-generated code>") generatedSrcLoc
        , testCase "mkBadSrcLoc" $ assertEqual "UnhelpfulLoc" (UnhelpfulLoc "bad") (mkBadSrcLoc "bad")
        , testCase "Eq" $ do
            assertEqual "==" (mkSrcLoc "f.lml" 1 5) (mkSrcLoc "f.lml" 1 5)
            assertBool "/=" (mkSrcLoc "f.lml" 1 5 /= generatedSrcLoc)
        , testCase "Show" $ assertNonEmptyShow (mkSrcLoc "f.lml" 1 5 :: SrcLoc)
        ]
    , testGroup
        "RealSrcSpan"
        [ testCase "mkRealSrcSpan + lenses" $ do
            assertEqual "file" ("f.lml" :: Text) (spanAB ^. srcSpanFile)
            assertEqual "sLine" (1 :: Int) (spanAB ^. srcSpanSLine)
            assertEqual "sColumn" (5 :: Int) (spanAB ^. srcSpanSColumn)
            assertEqual "eLine" (3 :: Int) (spanAB ^. srcSpanELine)
            assertEqual "eColumn" (9 :: Int) (spanAB ^. srcSpanEColumn)
        , testCase "Eq" $ do
            assertEqual "==" (mkRealSrcSpan locA locB) spanAB
            assertBool "/=" (spanAB /= mkRealSrcSpan locB locA)
        , testCase "Show" $ assertNonEmptyShow spanAB
        ]
    , testGroup
        "SrcSpan"
        [ testCase "mkSrcSpan (real)" $
            assertEqual "RealSrcSpan" (RealSrcSpan spanAB) (mkSrcSpan (mkSrcLoc "f.lml" 1 5) (mkSrcLoc "f.lml" 3 9))
        , testCase "mkSrcSpan (unhelpful left)" $
            assertEqual
              "UnhelpfulSpan"
              (UnhelpfulSpan (UnhelpfulOther "bad"))
              (mkSrcSpan (mkBadSrcLoc "bad") (mkSrcLoc "f.lml" 1 5))
        , testCase "mkSrcSpan (unhelpful right)" $
            assertEqual
              "UnhelpfulSpan"
              (UnhelpfulSpan (UnhelpfulOther "bad"))
              (mkSrcSpan (mkSrcLoc "f.lml" 1 5) (mkBadSrcLoc "bad"))
        , testCase "generatedSrcSpan" $ assertEqual "UnhelpfulGenerated" (UnhelpfulSpan UnhelpfulGenerated) generatedSrcSpan
        , testCase "mkBadSrcSpan" $ assertEqual "UnhelpfulOther" (UnhelpfulSpan (UnhelpfulOther "bad")) (mkBadSrcSpan "bad")
        , testCase "combineSrcSpans (same file)" $
            assertEqual
              "combined"
              ( combineSrcSpans
                  (mkSrcSpan (mkSrcLoc "f.lml" 1 1) (mkSrcLoc "f.lml" 2 2))
                  (mkSrcSpan (mkSrcLoc "f.lml" 3 3) (mkSrcLoc "f.lml" 4 4))
              )
              (RealSrcSpan (mkRealSrcSpan (mkRealSrcLoc "f.lml" 1 1) (mkRealSrcLoc "f.lml" 4 4)))
        , testCase "combineSrcSpans (unhelpful)" $ do
            let s1 = RealSrcSpan spanAB
            assertEqual "left unhelpful" s1 (combineSrcSpans generatedSrcSpan s1)
            assertEqual "right unhelpful" s1 (combineSrcSpans s1 (mkBadSrcSpan "bad"))
        , testCase "combineSrcSpans (files differ)" $ do
            let s1 = RealSrcSpan spanAB
                s2 = RealSrcSpan (mkRealSrcSpan locOtherFile locOtherFile)
            assertEqual "bad span" (UnhelpfulSpan (UnhelpfulOther "<combineSrcSpans: files differ>")) (combineSrcSpans s1 s2)
        , testCase "Eq" $ do
            assertEqual
              "=="
              (mkSrcSpan (mkSrcLoc "f.lml" 1 5) (mkSrcLoc "f.lml" 3 9))
              (mkSrcSpan (mkSrcLoc "f.lml" 1 5) (mkSrcLoc "f.lml" 3 9))
            assertBool "/=" (generatedSrcSpan /= RealSrcSpan spanAB)
        , testCase "Show" $ assertNonEmptyShow (RealSrcSpan spanAB :: SrcSpan)
        ]
    , testGroup
        "GenLocated / Located"
        [ testCase "L + accessors" $ do
            let located = (L generatedSrcSpan "hello" :: Located Text)
            assertEqual "unLoc" "hello" (unLoc located)
            assertEqual "getLoc" generatedSrcSpan (getLoc located)
            assertEqual "_L" (generatedSrcSpan, "hello") (located ^. _L)
        , testCase "mkBadLocated" $
            assertEqual "mkBadLocated" (L (UnhelpfulSpan (UnhelpfulOther "bad")) (5 :: Int)) (mkBadLocated "bad" (5 :: Int))
        , testCase "combineLocs" $ do
            let s1 = RealSrcSpan spanAB
                s2 = generatedSrcSpan
            assertEqual "combineLocs" (combineSrcSpans s1 s2) (combineLocs (L s1 (1 :: Int)) (L s2 (2 :: Int)))
        , testCase "Eq compares contents only" $ do
            let a = L generatedSrcSpan (5 :: Int)
                b = L (mkBadSrcSpan "other") (5 :: Int)
                c = L generatedSrcSpan (6 :: Int)
            assertEqual "same content, different loc" a b
            assertBool "different content" (a /= c)
        , testCase "Show" $ assertNonEmptyShow (L generatedSrcSpan "hello" :: Located Text)
        ]
    ]

-- | Asserts that 'show'ing the value is non-empty (forcing the 'Show' method).
assertNonEmptyShow :: (Show a) => a -> Assertion
assertNonEmptyShow x =
  let s = show x
   in when (null s) (assertFailure "rendered empty")
