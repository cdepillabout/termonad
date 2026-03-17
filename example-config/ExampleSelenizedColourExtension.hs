{-# LANGUAGE OverloadedStrings #-}
-- | This is an example Termonad configuration that shows how to use the
-- Selenized colour scheme https://github.com/jan-warchol/selenized

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

selenizedBlack :: ColourConfig (AlphaColour Double)
selenizedBlack =
  defaultColourConfig
    -- Set the default foreground colour of text of the terminal.
    { foregroundColour = Set (createColour 185 185 185) -- fg_0
    , backgroundColour = Set (createColour  24  24  24) -- bg_0
    -- Set the extended palette that has 2 Vecs of 8 Selenized black palette colours
    , palette = ExtendedPalette selenizedBlack1 selenizedBlack2
    }
  where
    selenizedBlack1 :: List8 (AlphaColour Double)
    selenizedBlack1 = fromMaybe defaultStandardColours $ mkList8
      [ createColour  37  37  37 -- bg_1
      , createColour 237  74  70 -- red
      , createColour 112 180  51 -- green
      , createColour 219 179  45 -- yellow
      , createColour  54 138 235 -- blue
      , createColour 235 110 183 -- magenta
      , createColour  63 197 183 -- cyan
      , createColour 119 119 119 -- dim_0
      ]

    selenizedBlack2 :: List8 (AlphaColour Double)
    selenizedBlack2 = fromMaybe defaultStandardColours $ mkList8
      [ createColour  59  59  59 -- bg_2
      , createColour 255  94  86 -- br_red
      , createColour 131 199  70 -- br_green
      , createColour 239 197  65 -- br_yellow
      , createColour  79 156 254 -- br_blue
      , createColour 255 129 202 -- br_magenta
      , createColour  86 216 201 -- br_cyan
      , createColour 222 222 222 -- fg_1
      ]

selenizedDark :: ColourConfig (AlphaColour Double)
selenizedDark =
  defaultColourConfig
    -- Set the default foreground colour of text of the terminal.
    { foregroundColour = Set (createColour 173 188 188) -- fg_0
    , backgroundColour = Set (createColour  16  60  72) -- bg_0
    -- Set the extended palette that has 2 Vecs of 8 Selenized dark palette colours
    , palette = ExtendedPalette selenizedDark1 selenizedDark2
    }
  where
    selenizedDark1 :: List8 (AlphaColour Double)
    selenizedDark1 = fromMaybe defaultStandardColours $ mkList8
      [ createColour  23  73  86 -- bg_1
      , createColour 250  87  80 -- red
      , createColour 117 185  56 -- green
      , createColour 219 179  45 -- yellow
      , createColour  70 149 247 -- blue
      , createColour 242 117 190 -- magenta
      , createColour  65 199 185 -- cyan
      , createColour 114 137 143 -- dim_0
      ]

    selenizedDark2 :: List8 (AlphaColour Double)
    selenizedDark2 = fromMaybe defaultStandardColours $ mkList8
      [ createColour  50  91 102 -- bg_2
      , createColour 255 102  92 -- br_red
      , createColour 132 199  71 -- br_green
      , createColour 235 193  61 -- br_yellow
      , createColour  88 163 255 -- br_blue
      , createColour 255 132 205 -- br_magenta
      , createColour  83 214 199 -- br_cyan
      , createColour 202 216 217 -- fg_1
      ]

selenizedWhite :: ColourConfig (AlphaColour Double)
selenizedWhite =
  defaultColourConfig
    -- Set the default foreground colour of text of the terminal.
    { foregroundColour = Set (createColour  71  71  71) -- fg_0
    , backgroundColour = Set (createColour 255 255 255) -- bg_0
    -- Set the extended palette that has 2 Vecs of 8 Selenized white palette colours
    , palette = ExtendedPalette selenizedWhite1 selenizedWhite2
    }
  where
    selenizedWhite1 :: List8 (AlphaColour Double)
    selenizedWhite1 = fromMaybe defaultLightColours $ mkList8
      [ createColour 235 235 235 -- bg_1
      , createColour 214   0  12 -- red
      , createColour  29 151   0 -- green
      , createColour 196 151   0 -- yellow
      , createColour   0 100 228 -- blue
      , createColour 221  15 157 -- magenta
      , createColour   0 173 156 -- cyan
      , createColour 135 135 135 -- dim_0
      ]

    selenizedWhite2 :: List8 (AlphaColour Double)
    selenizedWhite2 = fromMaybe defaultLightColours $ mkList8
      [ createColour 205 205 205 -- bg_2
      , createColour 191   0   0 -- br_red
      , createColour   0 132   0 -- br_green
      , createColour 175 133   0 -- br_yellow
      , createColour   0  84 207 -- br_blue
      , createColour 199   0 139 -- br_magenta
      , createColour   0 154 138 -- br_cyan
      , createColour  40  40  40 -- fg_1
      ]

selenizedLight :: ColourConfig (AlphaColour Double)
selenizedLight =
  defaultColourConfig
    -- Set the default foreground colour of text of the terminal.
    { foregroundColour = Set (createColour  83 103 109) -- fg_0
    , backgroundColour = Set (createColour 251 243 219) -- bg_0
    -- Set the extended palette that has 2 Vecs of 8 Selenized light palette colours
    , palette = ExtendedPalette selenizedLight1 selenizedLight2
    }
  where
    selenizedLight1 :: List8 (AlphaColour Double)
    selenizedLight1 = fromMaybe defaultLightColours $ mkList8
      [ createColour 233 228 208 -- bg_1
      , createColour 210  33  45 -- red
      , createColour  72 145   0 -- green
      , createColour 173 137   0 -- yellow
      , createColour   0 114 212 -- blue
      , createColour 202  72 152 -- magenta
      , createColour   0 156 143 -- cyan
      , createColour 144 153 149 -- dim_0
      ]

    selenizedLight2 :: List8 (AlphaColour Double)
    selenizedLight2 = fromMaybe defaultLightColours $ mkList8
      [ createColour 207 206 190 -- bg_2
      , createColour 204  23  41 -- br_red
      , createColour  66 139   0 -- br_green
      , createColour 167 131   0 -- br_yellow
      , createColour   0 109 206 -- br_blue
      , createColour 196  67 146 -- br_magenta
      , createColour   0 151 138 -- br_cyan
      , createColour  58  77  83 -- fg_1
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
  myColourExt <- createColourExtension selenizedDark

  -- Update 'myTMConfig' with our colour extension.
  let newTMConfig = addColourExtension myTMConfig myColourExt

  -- Start Termonad with our updated 'TMConfig'.
  start newTMConfig
