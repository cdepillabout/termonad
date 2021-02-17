
module Main where

import Build_doctests (flags, pkgs, module_sources)
-- import Test.DocTest (doctest)
import System.Process (proc, waitForProcess, withCreateProcess)
import System.Exit (exitWith)

main :: IO ()
main = do
  -- doctest args
  -- TESTTEST
  withCreateProcess (proc "doctest" args) $ \_ _ _ h -> do
    exitCode <- waitForProcess h
    exitWith exitCode
  where
    args :: [String]
    args = flags ++ pkgs ++ module_sources
