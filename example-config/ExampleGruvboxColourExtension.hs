{-# LANGUAGE OverloadedStrings #-}
-- | This is an example Termonad configuration that shows how to use the
-- Gruvbox colour scheme https://github.com/morhetz/gruvbox

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

-- This is our Gruvbox dark 'ColourConfig'.  It holds all of our dark-related settings.
gruvboxDark :: ColourConfig (AlphaColour Double)
gruvboxDark =
  defaultColourConfig
    -- Set the default foreground colour of text of the terminal.
    { foregroundColour = Set (createColour 213 196 161) -- fg2
    , backgroundColour = Set (createColour  40  40  40) -- bg0
    -- Set the extended palette that has 2 Vecs of 8 Gruvbox palette colours
    , palette = ExtendedPalette gruvboxDark1 gruvboxDark2
    }
  where
    gruvboxDark1 :: List8 (AlphaColour Double)
    gruvboxDark1 = fromMaybe defaultStandardColours $ mkList8
      [ createColour  40  40  40 -- bg0
      , createColour 204  36  29 -- red.1
      , createColour 152 151  26 -- green.2
      , createColour 215 153  33 -- yellow.3
      , createColour  69 133 136 -- blue.4
      , createColour 177  98 134 -- purple.5
      , createColour 104 157 106 -- aqua.6
      , createColour 189 174 147 -- fg3
      ]

    gruvboxDark2 :: List8 (AlphaColour Double)
    gruvboxDark2 = fromMaybe defaultStandardColours $ mkList8
      [ createColour 124 111 100 -- bg4
      , createColour 251  71  52 -- red.9
      , createColour 184 187  38 -- green.10
      , createColour 250 189  47 -- yellow.11
      , createColour 131 165 152 -- blue.12
      , createColour 211 134 155 -- purple.13
      , createColour 142 192 124 -- aqua.14
      , createColour 235 219 178 -- fg1
      ]

-- This is our Gruvbox light 'ColourConfig'.  It holds all of our dark-related settings.
gruvboxLight :: ColourConfig (AlphaColour Double)
gruvboxLight =
  defaultColourConfig
    -- Set the default foreground colour of text of the terminal.
    { foregroundColour = Set (createColour  60  56  54) -- fg1
    , backgroundColour = Set (createColour 251 241 199) -- bg0
    -- Set the extended palette that has 2 Vecs of 8 Gruvbox palette colours
    , palette = ExtendedPalette gruvboxLight1 gruvboxLight2
    }
  where
    gruvboxLight1 :: List8 (AlphaColour Double)
    gruvboxLight1 = fromMaybe defaultLightColours $ mkList8
      [ createColour 251 241 199 -- bg0
      , createColour 204  36  29 -- red.1
      , createColour 152 151  26 -- green.2
      , createColour 215 153  33 -- yellow.3
      , createColour  69 133 136 -- blue.4
      , createColour 177  98 134 -- purple.5
      , createColour 104 157 106 -- aqua.6
      , createColour 102  82  84 -- fg3
      ]

    gruvboxLight2 :: List8 (AlphaColour Double)
    gruvboxLight2 = fromMaybe defaultLightColours $ mkList8
      [ createColour 168 153 132 -- bg4
      , createColour 157   0   6 -- red.9
      , createColour 121 116  14 -- green.10
      , createColour 181 118  20 -- yellow.11
      , createColour   7 102 120 -- blue.12
      , createColour 143  63 113 -- purple.13
      , createColour  66 123  88 -- aqua.14
      , createColour  60  56  54 -- fg1
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
  -- First, create the colour extension based on either Gruvboxmodules.
  myColourExt <- createColourExtension gruvboxDark

  -- Update 'myTMConfig' with our colour extension.
  let newTMConfig = addColourExtension myTMConfig myColourExt

  -- Start Termonad with our updated 'TMConfig'.
  start newTMConfig
