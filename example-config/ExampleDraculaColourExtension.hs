{-# LANGUAGE OverloadedStrings #-}
-- | This is an example Termonad configuration that shows how to use the
-- Dracula colour scheme https://draculatheme.com/

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

-- This is our Dracula 'ColourConfig'.
dracula :: ColourConfig (AlphaColour Double)
dracula =
  defaultColourConfig
    -- Set the default background & foreground colour of text of the terminal.
    { backgroundColour = Set (createColour  40  42  54)  -- black.0
    , foregroundColour = Set (createColour 248 248 242)  -- white.7
    -- Set the extended palette that has 2 Vecs of 8 Dracula palette colours
    , palette = ExtendedPalette draculaNormal draculaBright
    }
  where
    draculaNormal :: List8 (AlphaColour Double)
    draculaNormal = fromMaybe defaultStandardColours $ mkList8
      [ createColour  40  42  54 -- black.0
      , createColour 255  85  85 -- red.1
      , createColour  80 250 123 -- green.2
      , createColour 241 250 140 -- yellow.3
      , createColour 189 147 249 -- blue.4
      , createColour 255 121 198 -- magenta.5
      , createColour 139 233 253 -- cyan.6
      , createColour 191 191 191 -- white.7
      ]

    draculaBright :: List8 (AlphaColour Double)
    draculaBright = fromMaybe defaultStandardColours $ mkList8
      [ createColour  77  77  77 -- black.8
      , createColour 255 110 103 -- red.9
      , createColour  90 247 142 -- green.10
      , createColour 244 249 157 -- yellow.11
      , createColour 202 169 250 -- blue.12
      , createColour 255 146 208 -- magenta.13
      , createColour 154 237 254 -- cyan.14
      , createColour 230 230 230 -- white.15
      ]

fontConf =
  defaultFontConfig
    { fontFamily = "Monospace"
    , fontSize = FontSizePoints 12
    }

main :: IO ()
main = do
  -- First, create the colour extension based on either PaperColor modules.
  myColourExt <- createColourExtension dracula

  -- Update 'myTMConfig' with our colour extension.
  let newTMConfig = addColourExtension myTMConfig myColourExt

  -- Start Termonad with our updated 'TMConfig'.
  start newTMConfig
