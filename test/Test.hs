module Main where

import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = do
  tests <- testsIO
  defaultMain tests

testsIO :: IO TestTree
testsIO = do
  return $ testGroup "tests" []

