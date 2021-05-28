{-# LANGUAGE OverloadedStrings #-}
-- | This is an example Termonad configuration that shows how to use the
-- PaperColor colour scheme https://github.com/NLKNguyen/papercolor-theme

module Main where

import Data.Maybe (fromMaybe)
import Termonad
  ( CursorBlinkMode(CursorBlinkModeOn)
  , Option(Set)
  , ShowScrollbar(ShowScrollbarNever)
  , TMConfig
  , confirmExit
  , cursorBlinkMode
  , defaultConfigOptions
  , defaultTMConfig
  , options
  , showMenu
  , showScrollbar
  , start
  , FontConfig
  , FontSize(FontSizePoints)
  , defaultFontConfig
  , fontConfig
  , fontFamily
  , fontSize
  )
import Termonad.Config.Colour
  ( AlphaColour
  , ColourConfig
  , Palette(ExtendedPalette)
  , addColourExtension
  , createColour
  , createColourExtension
  , defaultColourConfig
  , defaultStandardColours
  , defaultLightColours
  , backgroundColour
  , foregroundColour
  , palette
  , List8
  , mkList8
  )

-- This is our main 'TMConfig'.  It holds all of the non-colour settings
-- for Termonad.
--
-- This shows how a few settings can be changed.
myTMConfig :: TMConfig
myTMConfig =
  defaultTMConfig
    { options =
        defaultConfigOptions
          { showScrollbar = ShowScrollbarNever
          , confirmExit = False
          , showMenu = False
          , cursorBlinkMode = CursorBlinkModeOn
          , fontConfig = fontConf
          }
    }

-- This is our PaperColor dark 'ColourConfig'.  It holds all of our dark-related settings.
paperColorDark :: ColourConfig (AlphaColour Double)
paperColorDark =
  defaultColourConfig
    -- Set the default background & foreground colour of text of the terminal.
    { backgroundColour = Set (createColour  28  28  28)  -- black.0
    , foregroundColour = Set (createColour 208 208 208)  -- white.7
    -- Set the extended palette that has 2 Vecs of 8 PaperColor palette colours
    , palette = ExtendedPalette paperColorNormal paperColorBright
    }
  where
    paperColorNormal :: List8 (AlphaColour Double)
    paperColorNormal = fromMaybe defaultStandardColours $ mkList8
      [ createColour  28  28  28 -- black.0
      , createColour 175   0  95 -- red.1
      , createColour  95 175   0 -- green.2
      , createColour 215 175  95 -- yellow.3
      , createColour  95 175 215 -- blue.4
      , createColour 128 128 128 -- purple.5
      , createColour 215 135  95 -- aqua.6
      , createColour 208 208 208 -- white.7
      ]

    paperColorBright :: List8 (AlphaColour Double)
    paperColorBright = fromMaybe defaultStandardColours $ mkList8
      [ createColour  88  88  88 -- black.8
      , createColour  95 175  95 -- red.9
      , createColour 175 215   0 -- green.10
      , createColour 175 135 215 -- yellow.11
      , createColour 255 175   0 -- blue.12
      , createColour 255  95 175 -- purple.13
      , createColour   0 175 175 -- aqua.14
      , createColour  95 135 135 -- white.15
      ]

-- This is our PaperColor light 'ColourConfig'.  It holds all of our light-related settings
paperColorLight :: ColourConfig (AlphaColour Double)
paperColorLight =
  defaultColourConfig
    -- Set the default background & foreground colour of text of the terminal.
    { backgroundColour = Set (createColour 238 238 238) -- black.0
    , foregroundColour = Set (createColour  68  68  68) -- white.7
    -- Set the extended palette that has 2 Vecs of 8 PaperColor palette colours
    , palette = ExtendedPalette paperColorNormal paperColorBright
    }
  where
    paperColorNormal :: List8 (AlphaColour Double)
    paperColorNormal = fromMaybe defaultLightColours $ mkList8
      [ createColour 238 238 238 -- black.0
      , createColour 175   0   0 -- red.1
      , createColour   0 135   0 -- green.2
      , createColour  95 135   0 -- yellow.3
      , createColour   0 135 175 -- blue.4
      , createColour 135 135 135 -- purple.5
      , createColour   0  95 135 -- aqua.6
      , createColour  68  68  68 -- white.7
      ]

    paperColorBright :: List8 (AlphaColour Double)
    paperColorBright = fromMaybe defaultLightColours $ mkList8
      [ createColour 188 188 188 -- black.8
      , createColour 215   0   0 -- red.9
      , createColour 215   0 135 -- green.10
      , createColour 135   0 175 -- yellow.11
      , createColour 215  95   0 -- blue.12
      , createColour 215  95   0 -- purple.13
      , createColour   0  95 175 -- aqua.14
      , createColour   0  95 135 -- white.15
      ]


-- This defines the font for the terminal.
fontConf :: FontConfig
fontConf =
  defaultFontConfig
    { fontFamily = "Monospace"
    , fontSize = FontSizePoints 12
    }

main :: IO ()
main = do
  -- First, create the colour extension based on either PaperColor modules.
  myColourExt <- createColourExtension paperColorLight

  -- Update 'myTMConfig' with our colour extension.
  let newTMConfig = addColourExtension myTMConfig myColourExt

  -- Start Termonad with our updated 'TMConfig'.
  start newTMConfig
