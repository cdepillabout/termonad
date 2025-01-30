{-# LANGUAGE CPP #-}

-- | Description : Read and write to the Preferences file
-- Copyright     : (c) Dennis Gosnell, 2023
-- License       : BSD3
-- Stability     : experimental
-- Portability   : POSIX
--
-- This module contains functions for reading and writing to the preferences file.
--
-- The preferences file is generally stored in
-- @~/.config/termonad/termonad.yaml@.  It stores run-time preferences that
-- have been set through the Preferences dialog.  Preferences are loaded on
-- app startup, but only if the @termonad.hs@ configuration file doesn't exist.

module Termonad.Preferences.File where

import Termonad.Prelude

import Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE, withExceptT)
import Data.Aeson (Result(..), fromJSON)
#if MIN_VERSION_aeson(2, 0, 0)
import qualified Data.Aeson.KeyMap as KeyMap
#endif
import qualified Data.ByteString as ByteString
import qualified Data.HashMap.Strict as HashMap
import Data.Text (pack)
import Data.Yaml (ParseException, ToJSON (toJSON), decodeFileEither, encode, prettyPrintParseException)
import Data.Yaml.Aeson (Value(..))
import System.Directory
  ( XdgDirectory(XdgConfig)
  , createDirectoryIfMissing
  , doesFileExist
  , getXdgDirectory
  )
import System.FilePath ((</>))
import Termonad.Types
  ( ConfigOptions
  , TMConfig(TMConfig, hooks, options, keys)
  , defaultConfigHooks
  , defaultConfigOptions
  , defaultConfigKeys
  )

-- $setup
--
-- >>> import Data.Aeson(object, (.=))

-- | Get the path to the preferences file @~\/.config\/termonad\/termonad.yaml@.
getPreferencesFile :: IO FilePath
getPreferencesFile = do
  -- Get the termonad config directory
  confDir <- getXdgDirectory XdgConfig "termonad"
  createDirectoryIfMissing True confDir
  pure $ confDir </> "termonad.yaml"

-- | Read the configuration for the preferences file
-- @~\/.config\/termonad\/termonad.yaml@. This file stores only the 'options' of
-- 'TMConfig' so 'hooks' are initialized with 'defaultConfigHooks'.  If the
-- file doesn't exist, create it with the default values.
--
-- Any options that do not exist will get initialized with values from
-- 'defaultConfigOptions'.
tmConfigFromPreferencesFile :: IO TMConfig
tmConfigFromPreferencesFile = do
  confFile <- getPreferencesFile
  -- If there is no preferences file we create it with the default values
  exists <- doesFileExist confFile
  unless exists $ writePreferencesFile confFile defaultConfigOptions
  -- Read the configuration file
  eitherOptions <- readFileWithDefaults confFile
  options <-
    case eitherOptions of
      Left err -> do
        hPutStrLn stderr $ "Error parsing file " <> pack confFile <> ": " <> err
        pure defaultConfigOptions
      Right options -> pure options
  pure TMConfig { options = options
                , hooks = defaultConfigHooks
                , keys = defaultConfigKeys
                }

-- | Read the 'ConfigOptions' out of a configuration file.
--
-- Merge the raw 'ConfigOptions' with 'defaultConfigOptions'.  This makes sure
-- that old versions of the configuration file will still be able to be read
-- even if new options are added to 'ConfigOptions' in new versions of
-- Termonad.
readFileWithDefaults :: FilePath -> IO (Either Text ConfigOptions)
readFileWithDefaults file = runExceptT $ do
  -- Read the configuration file as a JSON object
  optsFromFile :: Value <-
    withExceptT parseExceptionToText . ExceptT $ decodeFileEither file
  let optsDefault :: Value = toJSON defaultConfigOptions
  -- Then merge it with the default options in JSON before converting it to
  -- a 'ConfigOptions'
  resultToExcept . fromJSON $ mergeObjVals optsFromFile optsDefault
  where
    parseExceptionToText :: ParseException -> Text
    parseExceptionToText = pack . prettyPrintParseException

    resultToExcept :: Result a -> ExceptT Text IO a
    resultToExcept (Success v) = pure v
    resultToExcept (Error str) = throwE (pack str)

-- | Merge 'Value's recursively.
--
-- This merges 'Value's recursively in 'Object' values, taking values that
-- have been explicitly over the defaults.  The defaults are only used if
-- there is no value that has been explicitly set.
--
-- For 'Array', 'String', 'Number', 'Bool', and 'Null', take the first 'Value'
-- (the one that has been explicitly set in the user's config file):
--
-- >>> mergeObjVals (Array [Number 1, Number 2]) (Array [String "hello"])
-- Array [Number 1.0,Number 2.0]
-- >>> mergeObjVals (String "hello") (String "bye")
-- String "hello"
-- >>> mergeObjVals (Number 1) (Number 2)
-- Number 1.0
-- >>> mergeObjVals (Bool True) (Bool False)
-- Bool True
-- >>> mergeObjVals Null Null
-- Null
--
-- Note that 'Value's in 'Array's are not recursed into:
--
-- >>> let obj1 = object ["hello" .= Number 2]
-- >>> let obj2 = object ["hello" .= String "bye"]
-- >>> mergeObjVals (Array [obj1]) (Array [obj2])
-- Array [Object (fromList [("hello",Number 2.0)])]
--
-- 'Object's are recursed into.  Unique keys from both Maps will be used.
-- Keys that are in both Maps will be merged according to the rules above:
--
-- >>> let object1 = object ["hello" .= Number 1, "bye" .= Number 100]
-- >>> let object2 = object ["hello" .= Number 2, "goat" .= String "chicken"]
-- >>> mergeObjVals object1 object2
-- Object (fromList [("bye",Number 100.0),("goat",String "chicken"),("hello",Number 1.0)])
--
-- 'Value's of different types will use the second 'Value':
--
-- >>> mergeObjVals Null (String "bye")
-- String "bye"
-- >>> mergeObjVals (Bool True) (Number 2)
-- Number 2.0
-- >>> mergeObjVals (Object mempty) (Bool False)
-- Bool False
mergeObjVals
  :: Value
     -- ^ Value that has been set explicitly in the User's configuration
     -- file.
  -> Value
     -- ^ Default value that will be used if no explicitly set value.
  -> Value
     -- ^ Merged values.
mergeObjVals optsFromFile optsDefault =
  case (optsFromFile, optsDefault) of
    -- Both the options from the file and the default options are an Object
    -- here.  Recursively merge the keys and values.
    (Object optsFromFileKeyMap, Object optsDefaultKeyMap) ->
      let
#if MIN_VERSION_aeson(2, 0, 0)
          hashMapFromKeyMap = KeyMap.toHashMap
          keyMapFromHashMap = KeyMap.fromHashMap
#else
          hashMapFromKeyMap = id
          keyMapFromHashMap = id
#endif
          optsFromFileHashMap = hashMapFromKeyMap optsFromFileKeyMap
          optsDefaultHashMap = hashMapFromKeyMap optsDefaultKeyMap
          optsResultHashMap = HashMap.unionWith mergeObjVals
                                optsFromFileHashMap
                                optsDefaultHashMap
          optsResultKeyMap = keyMapFromHashMap optsResultHashMap
      in Object optsResultKeyMap
    -- Both the value from the file and the default value are the same type.
    -- Use the value from the file.
    --
    -- XXX: This will end up causing readFileWithDefaults to fail if the value
    -- from the file is old and can no longer properly be decoded into a value
    -- expected by ConfigOptions.
    (Array fromFile, Array _) -> Array fromFile
    (String fromFile, String _) -> String fromFile
    (Number fromFile, Number _) -> Number fromFile
    (Bool fromFile, Bool _) -> Bool fromFile
    (Null, Null) -> Null
    -- The value from the file and the default value are different types. Just
    -- use the default value.
    (_, defVal) -> defVal

writePreferencesFile :: FilePath -> ConfigOptions -> IO ()
writePreferencesFile confFile options = do
  let yaml = encode options
      yamlWithComment =
        "# DO NOT EDIT THIS FILE BY HAND!\n" <>
        "#\n" <>
        "# This file is generated automatically by the Preferences dialog\n" <>
        "# in Termonad.  Please open the Preferences dialog if you wish to\n" <>
        "# modify this file.\n" <>
        "#\n" <>
        "# The settings in this file will be ignored if you have a\n" <>
        "# termonad.hs file in this same directory.\n\n" <>
        yaml
  ByteString.writeFile confFile yamlWithComment

-- | Save the configuration to the preferences file
-- @~\/.config\/termonad\/termonad.yaml@
saveToPreferencesFile :: TMConfig -> IO ()
saveToPreferencesFile TMConfig { options = options } = do
  confFile <- getPreferencesFile
  writePreferencesFile confFile options

