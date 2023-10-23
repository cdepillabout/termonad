-- | Module    : Termonad.Config
-- Description : Termonad Configuration Options
-- Copyright   : (c) Dennis Gosnell, 2018
-- License     : BSD3
-- Stability   : experimental
-- Portability : POSIX
--
-- This module exposes termonad's basic configuration options. To set these
-- options in your config, first ensure you've imported "Termonad".
--
-- Then for your main, apply 'Termonad.start' or 'Termonad.defaultMain' to a 'TMConfig' value.
-- We suggest you build such values by performing record updates on the
-- 'defaultTMConfig' rather than using the 'TMConfig' constructor, as the latter
-- is much more likely to break when there are changes to the 'TMConfig' type.
--
-- E.g.
--
-- @
--  -- Re-exports this module.
--  import "Termonad"
--
--  main :: IO ()
--  main = 'start' $ 'defaultTMConfig'
--    { 'options' = 'defaultConfigOptions'
--      { 'showScrollbar' = 'ShowScrollbarNever'
--      , 'confirmExit' = False
--      , 'showMenu' = False
--      , 'cursorBlinkMode' = 'CursorBlinkModeOff'
--      }
--    }
-- @
--
-- Additional options can be found in the following modules.
--
--  * "Termonad.Config.Colour"
--
-- If you want to see an example configuration file, as well as an explanation
-- for how to use Termonad, see the Termonad
-- <https://github.com/cdepillabout/termonad#configuring-termonad README>.

module Termonad.Config
  ( -- * Main Config Data Type
    TMConfig(..)
  , defaultTMConfig
  , ConfigOptions(..)
  , defaultConfigOptions
  , ConfigHooks(..)
  , defaultConfigHooks
  -- * Fonts
  , FontSize(..)
  , defaultFontSize
  , FontConfig(..)
  , defaultFontConfig
  -- * Misc
  , Option(..)
  , ShowScrollbar(..)
  , ShowTabBar(..)
  , CursorBlinkMode(..)
  , tmConfigFromPreferencesFile
  ) where

import GI.Vte (CursorBlinkMode(..))

import Termonad.Preferences (tmConfigFromPreferencesFile)
import Termonad.Types
