
module Main where

import Build_doctests (flags, pkgs, module_sources)
import Control.Concurrent (MVar, forkFinally, forkOS, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException)
import Control.Monad (void)
import System.Exit (exitWith)
import System.Process (proc, waitForProcess, withCreateProcess)
import System.Posix.Process (forkProcess)
import Test.DocTest (doctest)

-- Original thing that segfaults.
main :: IO ()
main = do
  doctest args
  where
    args :: [String]
    args = flags ++ pkgs ++ module_sources

-- This works, but fails when run with cabal because cabal doesn't
-- put the doctest binary on the PATH.
-- main :: IO ()
-- main = do
--   withCreateProcess (proc "doctest" args) $ \_ _ _ h -> do
--     exitCode <- waitForProcess h
--     exitWith exitCode
--   where
--     args :: [String]
--     args = flags ++ pkgs ++ module_sources

-- This is using forkIO, but it still segfaults.
-- main :: IO ()
-- main = do
--   mvar <- newEmptyMVar
--   void $ forkFinally (doctest args) (go mvar)
--   takeMVar mvar
--   print "finally finished"
--   where
--     args :: [String]
--     args = flags ++ pkgs ++ module_sources

--     go :: MVar () -> Either SomeException () -> IO ()
--     go mvar res = do
--       case res of
--         Left except -> do
--           print "got error"
--           print except
--         Right () -> do
--           print "finished correctly"
--       putMVar mvar ()

-- Fork a full OS process.  This also segfaults.
-- main :: IO ()
-- main = do
--   mvar <- newEmptyMVar
--   void $ forkProcess (go mvar)
--   takeMVar mvar
--   print "finally finished"
--   where
--     args :: [String]
--     args = flags ++ pkgs ++ module_sources

--     go :: MVar () -> IO ()
--     go mvar = do
--       print "starting doctests"
--       doctest args
--       print "finished doctests"
--       putMVar mvar ()
