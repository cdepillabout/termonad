{-# LANGUAGE OverloadedStrings #-}
-- | This is an example Termonad configuration that shows how to use the
-- One Dark Pro colour scheme https://binaryify.github.io/OneDark-Pro/
-- 
-- See an example in @../img/termonad-One_Dark_Pro.png@.

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
onedarkpro :: ColourConfig (AlphaColour Double)
onedarkpro =
  defaultColourConfig
    -- Set the default background & foreground colour of text of the terminal.
    { backgroundColour = Set (createColour  40  44  52)  -- black.0
    , foregroundColour = Set (createColour 171 178 191)  -- white.7
    -- Set the extended palette that has 2 Vecs of 8 Dracula palette colours
    , palette = ExtendedPalette onedarkproNormal onedarkproBright
    }
  where
    onedarkproNormal :: List8 (AlphaColour Double)
    onedarkproNormal = fromMaybe defaultStandardColours $ mkList8
      [ createColour  40  44  52 -- black.0
      , createColour 244 108 117 -- red.1
      , createColour 152 195 121 -- green.2
      , createColour 229 192 123 -- yellow.3
      , createColour  97 175 239 -- blue.4
      , createColour 198 120 221 -- magenta.5
      , createColour  86 182 194 -- cyan.6
      , createColour 171 178 191 -- white.7
      ]

    onedarkproBright :: List8 (AlphaColour Double)
    onedarkproBright = fromMaybe defaultStandardColours $ mkList8
      [ createColour  63  63  63 -- black.8
      , createColour 224 108 117 -- red.9
      , createColour 152 195 121 -- green.10
      , createColour 229 192 123 -- yellow.11
      , createColour  97 175 239 -- blue.12
      , createColour 198 120 221 -- magenta.13
      , createColour  86 182 194 -- cyan.14
      , createColour 191 197 206 -- white.15
      ]

fontConf =
  defaultFontConfig
    { fontFamily = "Monospace"
    , fontSize = FontSizePoints 12
    }

main :: IO ()
main = do
  -- First, create the colour extension based on either PaperColor modules.
  myColourExt <- createColourExtension onedarkpro

  -- Update 'myTMConfig' with our colour extension.
  let newTMConfig = addColourExtension myTMConfig myColourExt

  -- Start Termonad with our updated 'TMConfig'.
  start newTMConfig
