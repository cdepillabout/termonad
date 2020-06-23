{-# LANGUAGE OverloadedStrings #-}
-- | This is an example Termonad configuration that shows how to use the
-- Solarized colour scheme https://ethanschoonover.com/solarized/

module Main where

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

-- This is our Solarized dark 'ColourConfig'.  It holds all of our dark-related settings.
solarizedDark :: ColourConfig (AlphaColour Double)
solarizedDark =
  defaultColourConfig
    -- Set the default foreground colour of text of the terminal.
    { foregroundColour = Set (createColour 131 148 150) -- base0
    , backgroundColour = Set (createColour   0  43  54) -- base03
    -- Set the extended palette that has 2 Vecs of 8 Solarized palette colours
    , palette = ExtendedPalette solarizedDark1 solarizedDark2
    }
  where
    solarizedDark1 :: List8 (AlphaColour Double)
    solarizedDark1 = maybe defaultStandardColours id $ mkList8
      [ createColour   7  54  66 -- base02
      , createColour 220  50  47 -- red
      , createColour 133 153   0 -- green
      , createColour 181 137   0 -- yellow
      , createColour  38 139 210 -- blue
      , createColour 211  54 130 -- magenta
      , createColour  42 161 152 -- cyan
      , createColour 238 232 213 -- base2
      ]

    solarizedDark2 :: List8 (AlphaColour Double)
    solarizedDark2 = maybe defaultStandardColours id $ mkList8
      [ createColour   0  43  54 -- base03
      , createColour 203  75  22 -- orange
      , createColour  88 110 117 -- base01
      , createColour 101 123 131 -- base00
      , createColour 131 148 150 -- base0
      , createColour 108 113 196 -- violet
      , createColour 147 161 161 -- base1
      , createColour 253 246 227 -- base3
      ]

-- This is our Solarized light 'ColourConfig'.  It holds all of our light-related settings.
solarizedLight :: ColourConfig (AlphaColour Double)
solarizedLight =
  defaultColourConfig
    -- Set the default foreground colour of text of the terminal.
    { foregroundColour = Set (createColour 101 123 131) -- base00
    , backgroundColour = Set (createColour 253 246 227) -- base3
    -- Set the extended palette that has 2 Vecs of 8 Solarized palette colours
    , palette = ExtendedPalette solarizedLight1 solarizedLight2
    }
  where
    solarizedLight1 :: List8 (AlphaColour Double)
    solarizedLight1 = maybe defaultLightColours id $ mkList8
      [ createColour   7  54  66 -- base02
      , createColour 220  50  47 -- red
      , createColour 133 153   0 -- green
      , createColour 181 137   0 -- yellow
      , createColour  38 139 210 -- blue
      , createColour 211  54 130 -- magenta
      , createColour  42 161 152 -- cyan
      , createColour 238 232 213 -- base2
      ]

    solarizedLight2 :: List8 (AlphaColour Double)
    solarizedLight2 = maybe defaultLightColours id $ mkList8
      [ createColour   0  43  54 -- base03
      , createColour 203  75  22 -- orange
      , createColour  88 110 117 -- base01
      , createColour 101 123 131 -- base00
      , createColour 131 148 150 -- base0
      , createColour 108 113 196 -- violet
      , createColour 147 161 161 -- base1
      , createColour 253 246 227 -- base3
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
  -- First, create the colour extension based on either Solarized modules.
  myColourExt <- createColourExtension solarizedDark

  -- Update 'myTMConfig' with our colour extension.
  let newTMConfig = addColourExtension myTMConfig myColourExt

  -- Start Termonad with our updated 'TMConfig'.
  start newTMConfig
