-- This file comes from cabal-doctest:
-- https://github.com/phadej/cabal-doctest/blob/master/simple-example
--
-- It is needed so that doctest can be run with the same options as the modules
-- are compiled with.

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Distribution.PackageDescription (HookedBuildInfo, cppOptions, emptyBuildInfo)
import Distribution.Simple (Args, UserHooks, defaultMainWithHooks, preBuild, simpleUserHooks)
import Distribution.Simple.Program (configureProgram, defaultProgramConfiguration, getDbProgramOutput, pkgConfigProgram)
import Distribution.Simple.Setup (BuildFlags)
import Distribution.Verbosity (verbose)

#ifndef MIN_VERSION_cabal_doctest
#define MIN_VERSION_cabal_doctest(x,y,z) 0
#endif

#if MIN_VERSION_cabal_doctest(1,0,0)

import Distribution.Extra.Doctest (addDoctestsUserHook)
main :: IO ()
main =
  defaultMainWithHooks . addPkgConfigGtkUserHook $
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
main = defaultMainWithHooks $ addPkgConfigGtkUserHook simpleUserHooks

#endif

addPkgConfigGtkUserHook :: UserHooks -> UserHooks
addPkgConfigGtkUserHook oldUserHooks =
  oldUserHooks
    { preBuild = pkgConfigGtkPreBuildHook $ preBuild oldUserHooks
    }

pkgConfigGtkPreBuildHook :: (Args -> BuildFlags -> IO HookedBuildInfo) -> Args -> BuildFlags -> IO HookedBuildInfo
pkgConfigGtkPreBuildHook oldFunc args buildFlags = do
  (maybeOldLibBuildInfo, oldExesBuildInfo) <- oldFunc args buildFlags
  case maybeOldLibBuildInfo of
    Just oldLibBuildInfo -> do
      putStrLn "In Setup.hs, in pkgConfigGtkPreBuildHook, oldLibBuildInfo is not Nothing:"
      print oldLibBuildInfo
      putStrLn "\nDon't know how to proceed."
      error "Build info not Nothing."
    Nothing -> do
      pkgDb <-
        configureProgram verbose pkgConfigProgram defaultProgramConfiguration
      pkgConfigOutput <-
        getDbProgramOutput
          verbose
          pkgConfigProgram
          pkgDb
          ["--modversion", "gtk+-3.0"]
      print pkgConfigOutput
      let maybeGtkVersion = readMaybe pkgConfigOutput
      -- Drop the newline on the end of the pkgConfigOutput.
      -- This should give us a version number like @3.22.11@.
      let versionNum = reverse $ drop 1 $ reverse pkgConfigOutput
      let newLibBuildInfo =
            emptyBuildInfo
              { cppOptions = ["-DFOO_BAR_BAZ2"]
              }
      pure (Just newLibBuildInfo, oldExesBuildInfo)
