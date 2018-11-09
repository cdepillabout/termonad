
module Main where

import Termonad.Prelude

import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = do
  tests <- testsIO
  defaultMain tests

testsIO :: IO TestTree
testsIO = do
  pure $
    testGroup
      "tests"
      [ -- focusListTests
      ]
