
module Main where

import Termonad.Prelude

import Test.Tasty (TestTree, defaultMain, testGroup)

import Test.FocusList (focusListTestsIO)

main :: IO ()
main = do
  tests <- testsIO
  defaultMain tests

testsIO :: IO TestTree
testsIO = do
  focusListTests <- focusListTestsIO
  pure $
    testGroup
      "tests"
      [ focusListTests
      ]
