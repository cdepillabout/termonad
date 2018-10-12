
module Termonad.Config
  ( TMConfig(..)
  , defaultTMConfig
  , FontSize(..)
  , defaultFontSize
  , FontConfig(..)
  , defaultFontConfig
  , Option(..)
  , ShowScrollbar(..)
  , ShowTabBar(..)
  , CursorBlinkMode(..)
  ) where

import Termonad.Prelude hiding ((\\), index)
import Termonad.Types

import GI.Vte (CursorBlinkMode(..))

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
    , extension = SomeConfigExtension ()
    }
