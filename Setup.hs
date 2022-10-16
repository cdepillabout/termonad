-- This file comes from cabal-doctest:
-- https://github.com/phadej/cabal-doctest/blob/master/simple-example
--
-- It is needed so that doctest can be run with the same options as the modules
-- are compiled with.

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Data.Maybe (catMaybes)
import Distribution.PackageDescription (HookedBuildInfo, cppOptions, emptyBuildInfo)
import Distribution.Simple (UserHooks, defaultMainWithHooks, preBuild, preRepl, simpleUserHooks)
import Distribution.Simple.Program (configureProgram, defaultProgramDb, getDbProgramOutput, pkgConfigProgram)
import Distribution.Text (simpleParse)
import Distribution.Verbosity (normal)
import Distribution.Version (Version, mkVersion)

#ifndef MIN_VERSION_cabal_doctest
#define MIN_VERSION_cabal_doctest(x,y,z) 0
#endif

#if MIN_VERSION_cabal_doctest(1,0,0)

import Distribution.Extra.Doctest (addDoctestsUserHook)
main :: IO ()
main = do
  cppOpts <- getTermonadCPPOpts
  defaultMainWithHooks . addPkgConfigTermonadUserHook cppOpts $
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
  cppOpts <- getTermonadCPPOpts
  defaultMainWithHooks $ addPkgConfigTermonadUserHook cppOpts simpleUserHooks

#endif

-- | Add CPP macros representing the version of the GTK and VTE system
-- libraries.
addPkgConfigTermonadUserHook :: [String] -> UserHooks -> UserHooks
addPkgConfigTermonadUserHook cppOpts oldUserHooks = do
  oldUserHooks
    { preBuild = pkgConfigTermonadHook cppOpts $ preBuild oldUserHooks
    , preRepl = pkgConfigTermonadHook cppOpts (preRepl oldUserHooks)
    }

pkgConfigTermonadHook :: [String] -> (args -> flags -> IO HookedBuildInfo) -> args -> flags -> IO HookedBuildInfo
pkgConfigTermonadHook cppOpts oldFunc args flags = do
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

getTermonadCPPOpts :: IO [String]
getTermonadCPPOpts = do
  gtk <- getGtkVersionCPPOpts
  vte <- getVteVersionCPPOpts
  pure $ gtk <> vte

getVteVersionCPPOpts :: IO [String]
getVteVersionCPPOpts = do
  maybeVers <- getPkgConfigVersionFor "vte-2.91"
  case maybeVers of
    Nothing -> pure []
    Just vers -> pure $ createVteVersionCPPOpts vers

getGtkVersionCPPOpts :: IO [String]
getGtkVersionCPPOpts = do
  maybeVers <- getPkgConfigVersionFor "gtk+-3.0"
  case maybeVers of
    Nothing -> pure []
    Just vers -> pure $ createGtkVersionCPPOpts vers

-- | Just like 'createGtkVersionCPPOpts' but for VTE instead of GTK.
createVteVersionCPPOpts
  :: Version
  -> [String]
createVteVersionCPPOpts vers =
  catMaybes
    [ if vers >= mkVersion [0,44] then Just "-DVTE_VERSION_GEQ_0_44" else Nothing
    , if vers >= mkVersion [0,63] then Just "-DVTE_VERSION_GEQ_0_63" else Nothing
    ]

-- | Based on the version of the GTK3 library as reported by @pkg-config@, return
-- a list of CPP macros that contain the GTK version.  These can be used in the
-- Haskell code to work around differences in the gi-gtk library Haskell
-- library when compiled against different versions of the GTK system library.
--
-- This list may need to be added to.
createGtkVersionCPPOpts
  :: Version  -- ^ 'Version' of the GTK3 library as reported by @pkg-config@.
  -> [String] -- ^ A list of CPP macros to show the GTK version.
createGtkVersionCPPOpts gtkVersion =
  catMaybes
    [ if gtkVersion >= mkVersion [3,22] then Just "-DGTK_VERSION_GEQ_3_22" else Nothing
    ]

getPkgConfigVersionFor :: String -> IO (Maybe Version)
getPkgConfigVersionFor program = do
  pkgDb <- configureProgram normal pkgConfigProgram defaultProgramDb
  pkgConfigOutput <-
    getDbProgramOutput normal pkgConfigProgram pkgDb ["--modversion", program]
  -- Drop the newline on the end of the pkgConfigOutput.
  -- This should give us a version number like @3.22.11@.
  let rawProgramVersion = reverse $ drop 1 $ reverse pkgConfigOutput
  let maybeProgramVersion = simpleParse rawProgramVersion
  case maybeProgramVersion of
    Nothing -> do
      putStrLn $
        "In Setup.hs, in getPkgConfigVersionFor, could not parse " <>
        program <> " version: " <> show pkgConfigOutput
      putStrLn $
        "\nNot defining any CPP macros based on the version of the system " <>
        program <> " library."
      putStrLn "\nCompilation of termonad may fail."
      pure Nothing
    Just programVersion -> pure $ Just programVersion
