
module Termonad.PreferencesFile where

import Termonad.Prelude

import Data.Yaml
  ( decodeFileEither
  , encodeFile
  , prettyPrintParseException
  , withText
  )
import System.Directory (XdgDirectory(XdgConfig), createDirectoryIfMissing, doesFileExist, getXdgDirectory)

import Termonad.Types (TMConfig(TMConfig, hooks, options), defaultConfigHooks, defaultConfigOptions)

-- | Get the path to the preferences file @~\/.config\/termonad\/termonad.yml@.
getPreferencesFile :: IO FilePath
getPreferencesFile = do
  -- Get the termonad config directory
  confDir <- getXdgDirectory XdgConfig "termonad"
  createDirectoryIfMissing True confDir
  pure $ confDir </> "termonad.yaml"

-- | Read the configuration for the preferences file
-- @~\/.config\/termonad\/termonad.yml@. This file stores only the 'options' of
-- 'TMConfig' so 'hooks' are initialized with 'defaultConfigHooks'.  If the
-- file doesn't exist, create it with the default values.
tmConfigFromPreferencesFile :: IO TMConfig
tmConfigFromPreferencesFile = do
  confFile <- getPreferencesFile
  -- If there is no preferences file we create it with the default values
  exists <- doesFileExist confFile
  unless exists $ encodeFile confFile defaultConfigOptions
  -- Read the configuration file
  eitherOptions <- decodeFileEither confFile
  options <-
    case eitherOptions of
      Left err -> do
        hPutStrLn stderr $ "Error parsing file " <> pack confFile
        hPutStrLn stderr $ pack $ prettyPrintParseException err
        pure defaultConfigOptions
      Right options -> pure options
  pure $ TMConfig { options = options, hooks = defaultConfigHooks }

-- | Save the configuration to the preferences file
-- @~\/.config\/termonad\/termonad.yml@
saveToPreferencesFile :: TMConfig -> IO ()
saveToPreferencesFile TMConfig { options = options } = do
  confFile <- getPreferencesFile
  encodeFile confFile options
