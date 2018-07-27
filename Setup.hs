-- This file comes from cabal-doctest:
-- https://github.com/phadej/cabal-doctest/blob/master/simple-example
--
-- It is needed so that doctest can be run with the same options as the modules
-- are compiled with.

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Data.Maybe (catMaybes)
import Data.Version (Version)
import Distribution.PackageDescription (HookedBuildInfo, cppOptions, emptyBuildInfo)
import Distribution.Simple (UserHooks, defaultMainWithHooks, preBuild, preRepl, simpleUserHooks)
import Distribution.Simple.Program (configureProgram, defaultProgramConfiguration, getDbProgramOutput, pkgConfigProgram)
import Distribution.Text (simpleParse)
import Distribution.Verbosity (verbose)

#ifndef MIN_VERSION_cabal_doctest
#define MIN_VERSION_cabal_doctest(x,y,z) 0
#endif

#if MIN_VERSION_cabal_doctest(1,0,0)

import Distribution.Extra.Doctest (addDoctestsUserHook)
main :: IO ()
main = do
  cppOpts <- getGtkVersionCPPOpts
  defaultMainWithHooks . addPkgConfigGtkUserHook cppOpts $
    addDoctestsUserHook "doctests" simpleUserHooks

#else

#ifdef MIN_VERSION_Cabal
-- If the macro is defined, we have new cabal-install,
-- but for some reason we don't have cabal-doctest in package-db
--
-- Probably we are running cabal sdist, when otherwise using new-build
-- workflow
#warning You are configuring this package without cabal-doctest installed. \
         The doctests test-suite will not work as a result. \
         To fix this, install cabal-doctest before configuring.
#endif

main :: IO ()
main = do
  cppOpts <- getGtkVersionCPPOpts
  defaultMainWithHooks $ addPkgConfigGtkUserHook cppOpts simpleUserHooks

#endif

-- | Add CPP macros representing the version of the GTK system library.
addPkgConfigGtkUserHook :: [String] -> UserHooks -> UserHooks
addPkgConfigGtkUserHook cppOpts oldUserHooks = do
  oldUserHooks
    { preBuild = pkgConfigGtkHook cppOpts $ preBuild oldUserHooks
    , preRepl = pkgConfigGtkHook cppOpts $ preRepl oldUserHooks
    }

pkgConfigGtkHook :: [String] -> (args -> flags -> IO HookedBuildInfo) -> args -> flags -> IO HookedBuildInfo
pkgConfigGtkHook cppOpts oldFunc args flags = do
  (maybeOldLibHookedInfo, oldExesHookedInfo) <- oldFunc args flags
  case maybeOldLibHookedInfo of
    Just oldLibHookedInfo -> do
      let newLibHookedInfo =
            oldLibHookedInfo
              { cppOptions = cppOptions oldLibHookedInfo <> cppOpts
              }
      pure (Just newLibHookedInfo, oldExesHookedInfo)
    Nothing -> do
      let newLibHookedInfo =
            emptyBuildInfo
              { cppOptions = cppOpts
              }
      pure (Just newLibHookedInfo, oldExesHookedInfo)

getGtkVersionCPPOpts :: IO [String]
getGtkVersionCPPOpts = do
  pkgDb <- configureProgram verbose pkgConfigProgram defaultProgramConfiguration
  pkgConfigOutput <-
    getDbProgramOutput
      verbose
      pkgConfigProgram
      pkgDb
      ["--modversion", "gtk+-3.0"]
  -- Drop the newline on the end of the pkgConfigOutput.
  -- This should give us a version number like @3.22.11@.
  let rawGtkVersion = reverse $ drop 1 $ reverse pkgConfigOutput
  let maybeGtkVersion = simpleParse rawGtkVersion
  case maybeGtkVersion of
    Nothing -> do
      putStrLn "In Setup.hs, in getGtkVersionCPPOpts, could not parse gtk version:"
      print pkgConfigOutput
      putStrLn "\nDon't know how to proceed."
      fail "Can't parse pkg-config output."
    Just gtkVersion -> do
      let cppOpts = createGtkVersionCPPOpts gtkVersion
      pure cppOpts

-- | Based on the version of the GTK3 library as reported by @pkg-config@, return
-- a list of CPP macros that contain the GTK version.  These can be used in the
-- Haskell code to work around differences in the gi-gtk library Haskell
-- library when compiled against different versions of the GTK system library.
--
-- This list may need to be added too.
createGtkVersionCPPOpts
  :: Version  -- ^ 'Version' of the GTK3 library as reported by @pkg-config@.
  -> [String] -- ^ A list of CPP macros to show the GTK version.
createGtkVersionCPPOpts gtkVersion =
  catMaybes $
    [ if gtkVersion >= [3,22,20] then Just "-DGTK_VERSION_GEQ_3_22_20" else Nothing
    ]
