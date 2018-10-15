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
-- Then for your main, apply 'start' or 'defaultMain' to a 'TMConfig' value.
-- We suggest you build such values by performing record updates on the
-- 'defaultTMConfig' rather than using the 'TMConfig' constructor, as the latter
-- is much more likely to break when there are changes to the 'TMConfig' type.
--
-- E.g.
--
-- > -- Re-exports this module.
-- > import "Termonad"
-- >
-- > main :: IO ()
-- > main = 'start' $ 'defaultTMConfig'
-- >   { 'showScrollbar' = 'ShowScrollbarNever'
-- >   , 'confirmExit' = False
-- >   , 'showMenu' = False
-- >   , 'cursorBlinkMode' = 'CursorBlinkModeOff'
-- >   }
--
-- To use 'ConfigExtension's with additional options, see these modules:
--
--  * "Termonad.Config.Colour"
--
-- If you want to write your own extension, see "Termonad.Config.Extension".

module Termonad.Config (
  -- * Main Config Data Type
  TMConfig(..),
  defaultTMConfig,
  -- * Fonts
  FontSize(..),
  defaultFontSize,
  FontConfig(..),
  defaultFontConfig,
  -- * Misc
  Option(..),
  ShowScrollbar(..),
  ShowTabBar(..),
  CursorBlinkMode(..)
) where

import Termonad.Prelude hiding ((\\), index)

import GI.Vte (CursorBlinkMode(..))

import Termonad.Types

-- | The default 'FontSize' used if not specified.
--
-- >>> defaultFontSize
-- FontSizePoints 12
defaultFontSize :: FontSize
defaultFontSize = FontSizePoints 12

-- | The default 'FontConfig' to use if not specified.
--
-- >>> defaultFontConfig == FontConfig {fontFamily = "Monospace", fontSize = defaultFontSize}
-- True
defaultFontConfig :: FontConfig
defaultFontConfig =
  FontConfig
    { fontFamily = "Monospace"
    , fontSize = defaultFontSize
    }

-- | The default 'TMConfig'.
--
-- >>> :{
--   let defTMConf =
--         TMConfig
--           { fontConfig = defaultFontConfig
--           , showScrollbar = ShowScrollbarIfNeeded
--           , scrollbackLen = 10000
--           , confirmExit = True
--           , wordCharExceptions = "-#%&+,./=?@\\_~\183:"
--           , showMenu = True
--           , showTabBar = ShowTabBarIfNeeded
--           , cursorBlinkMode = CursorBlinkModeOn
--           , extension = defaultConfigExtension
--           }
--   in defaultTMConfig == defTMConf
-- :}
-- True
defaultTMConfig :: TMConfig
defaultTMConfig =
  TMConfig
    { fontConfig = defaultFontConfig
    , showScrollbar = ShowScrollbarIfNeeded
    , scrollbackLen = 10000
    , confirmExit = True
    , wordCharExceptions = "-#%&+,./=?@\\_~\183:"
    , showMenu = True
    , showTabBar = ShowTabBarIfNeeded
    , cursorBlinkMode = CursorBlinkModeOn
    , extension = defaultConfigExtension
    }
