
module Main where

import Build_doctests (flags, pkgs, module_sources)
import Test.DocTest (doctest)

main :: IO ()
main = do
  doctest args
  where
    args :: [String]
    args = flags ++ pkgs ++ module_sources
