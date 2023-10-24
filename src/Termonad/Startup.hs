
-- | Description : Functions for starting up Termonad.
-- Copyright     : (c) Dennis Gosnell, 2023
-- License       : BSD3
-- Stability     : experimental
-- Portability   : POSIX
--
-- This module contains functions for starting up Termonad.

module Termonad.Startup where

import Termonad.Prelude

import Config.Dyre (wrapMain, newParams)
import Control.Lens (over)
import System.IO.Error (doesNotExistErrorType, ioeGetErrorType, ioeGetFileName, tryIOError)
import Termonad.App (start)
import Termonad.Cli (parseCliArgs, applyCliArgs)
import Termonad.Lenses (lensOptions)
import Termonad.Types (TMConfig)


-- | Run Termonad with the given 'TMConfig'.
--
-- Do not perform any of the recompilation operations that the 'defaultMain'
-- function does.
--
-- This function __does__ parse command line arguments, and then calls 'Termonad.App.start'.
startWithCliArgs :: TMConfig -> IO ()
startWithCliArgs tmConfig = do
  cliArgs <- parseCliArgs
  start (over lensOptions (applyCliArgs cliArgs) tmConfig)

-- | Run Termonad with the given 'TMConfig'.
--
-- This function will check if there is a @~\/.config\/termonad\/termonad.hs@ file
-- and a @~\/.cache\/termonad\/termonad-linux-x86_64@ binary.  Termonad will
-- perform different actions based on whether or not these two files exist.
--
-- Here are the four different possible actions based on the existence of these
-- two files.
--
-- - @~\/.config\/termonad\/termonad.hs@ exists, @~\/.cache\/termonad\/termonad-linux-x86_64@ exists
--
--     The timestamps of these two files are checked.  If the
--     @~\/.config\/termonad\/termonad.hs@ file has been modified after the
--     @~\/.cache\/termonad\/termonad-linux-x86_64@ binary, then Termonad will use
--     GHC to recompile the @~\/.config\/termonad\/termonad.hs@ file, producing a
--     new binary at @~\/.cache\/termonad\/termonad-linux-x86_64@.  This new binary
--     will be re-executed.  The 'TMConfig' passed to this 'defaultMain' will be
--     effectively thrown away, however all command line options will be passed
--     on to this new Termonad process.
--
--     If GHC fails to recompile the @~\/.config\/termonad\/termonad.hs@ file, then
--     Termonad will just execute 'startWithCliArgs' with the 'TMConfig' passed in.
--
--     If the @~\/.cache\/termonad\/termonad-linux-x86_64@ binary has been modified
--     after the @~\/.config\/termonad\/termonad.hs@ file, then Termonad will
--     re-exec the @~\/.cache\/termonad\/termonad-linux-x86_64@ binary.  The
--     'TMConfig' passed to this 'defaultMain' will be effectively thrown away,
--     however all command line options will be passed on to this new Termonad
--     process.
--
-- - @~\/.config\/termonad\/termonad.hs@ exists, @~\/.cache\/termonad\/termonad-linux-x86_64@ does not exist
--
--     Termonad will use GHC to recompile the @~\/.config\/termonad\/termonad.hs@
--     file, producing a new binary at @~\/.cache\/termonad\/termonad-linux-x86_64@.
--     This new binary will be re-executed.  The 'TMConfig' passed to this
--     'defaultMain' will be effectively thrown away, however all command line
--     options will be passed on to this new Termonad process.
--
--     If GHC fails to recompile the @~\/.config\/termonad\/termonad.hs@ file, then
--     Termonad will just execute 'startWithCliArgs' with the 'TMConfig' passed in.
--
-- - @~\/.config\/termonad\/termonad.hs@ does not exist, @~\/.cache\/termonad\/termonad-linux-x86_64@ exists
--
--     Termonad will ignore the @~\/.cache\/termonad\/termonad-linux-x86_64@ binary
--     and just run 'startWithCliArgs' with the 'TMConfig' passed to this function.
--
-- - @~\/.config\/termonad\/termonad.hs@ does not exist, @~\/.cache\/termonad\/termonad-linux-x86_64@ does not exist
--
--     Termonad will run 'startWithCliArgs' with the 'TMConfig' passed to this function.
--
-- Other notes:
--
-- 1. That the locations of @~\/.config\/termonad\/termonad.hs@ and
--    @~\/.cache\/termonad\/termonad-linux-x86_64@ may differ depending on your
--    system.
--
-- 2. In your own @~\/.config\/termonad\/termonad.hs@ file, you can use either
--    'defaultMain' or 'startWithCliArgs'.  As long as you always
--    execute the system-wide @termonad@ binary (instead of the binary produced
--    as @~\/.cache\/termonad\/termonad-linux-x86_64@), the effect should be
--    similar.
--
-- 3. If you directly run the cached termonad binary (e.g.
--    @~\/.cache\/termonad\/termonad-linux-x86_64@) instead of the
--    system-installed Termonad binary (e.g. @\/usr\/bin\/termonad@), the Termonad
--    /will/ recompile the the configuration file
--    @~\/.config\/termonad\/termonad.hs@ according to the above logic (while
--    possibly overwriting the executable file for the binary you're currently
--    running), but it /will not/ re-exec into the newly built @termonad@ binary.
--
-- 4. When running the system-wide @termonad@ binary, the initial 'TMConfig'
--    that gets passed into this function comes from
--    'Termonad.PreferencesFile.tmConfigFromPreferencesFile'.  As stated above,
--    this initial 'TMConfig' gets ignored if users have a
--    @~\/.config\/termonad\/termonad.hs@ file that gets recompiled and re-execed.
--
--    End users generally call 'defaultMain' in their
--    @~\/.config\/termonad\/termonad.hs@ file.
--
--    'defaultMain' interally calls 'startWithCliArgs', which parses CLI arguments
--    and combines them with the passed-in 'TMConfig'.  'startWithCliArgs' then
--    internally calls 'Termonad.App.start'.
--
--    If you don't want the re-compiling and re-exec functionality, you can directly
--    use 'startWithCliArgs'.  If you also don't want the CLI argument parsing
--    functionality, you can directly use 'Termonad.App.start'.
defaultMain :: TMConfig -> IO ()
defaultMain tmConfig = do
  let params = newParams "termonad" realMainFunc collectErrs
  eitherRes <- tryIOError $ wrapMain params (tmConfig, "")
  case eitherRes of
    Left ioErr
      | ioeGetErrorType ioErr == doesNotExistErrorType && ioeGetFileName ioErr == Just "ghc" -> do
          putStrLn $
            "Could not find ghc on your PATH.  Ignoring your termonad.hs " <>
            "configuration file and running termonad with default settings."
          startWithCliArgs tmConfig
      | otherwise -> do
          putStrLn "IO error occurred when trying to run termonad:"
          print ioErr
          putStrLn "Don't know how to recover.  Exiting."
    Right _ -> pure ()
  where
    -- The real main function
    realMainFunc :: (TMConfig, String) -> IO ()
    realMainFunc (cfg, errs) =
      case errs of
        "" -> startWithCliArgs cfg
        _ -> do
          putStrLn $ "Errors from dyre when recompiling Termonad:" <> errs
          putStrLn "Continuing with Termonad without re-execing into recompiled binary..."
          startWithCliArgs cfg

    -- A function that can easily collect errors from dyre
    collectErrs :: (TMConfig, String) -> String -> (TMConfig, String)
    collectErrs (cfg, oldErrs) newErr = (cfg, oldErrs <> "\n" <> newErr)
