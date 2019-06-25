-- | This is an example Termonad configuration that shows how to use the
-- Solarized colour scheme https://ethanschoonover.com/solarized/


module Main where

import Termonad
  ( CursorBlinkMode(CursorBlinkModeOff), Option(Set)
  , ShowScrollbar(ShowScrollbarNever), TMConfig, confirmExit, cursorBlinkMode
  , defaultConfigOptions, defaultTMConfig, options, showMenu, showScrollbar
  , start
  )
import Termonad.Config.Colour
  ( Colour, ColourConfig, Palette(ExtendedPalette), addColourExtension
  , createColourExtension, cursorBgColour, defaultColourConfig, foregroundColour
  , palette, sRGB24
  )
import Termonad.Config.Vec (Vec((:*), EmptyVec), N8, unsafeFromListVec_)
import Data.Colour.SRGB (Colour, sRGB24)

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
          , cursorBlinkMode = CursorBlinkModeOff
          }
    }

-- This is our Solarized dark 'ColourConfig'.  It holds all of our dark-related settings.
solarizedDark :: ColourConfig (Colour Double)
solarizedDark =
  defaultColourConfig
    -- Set the default foreground colour of text of the terminal.
    { foregroundColour = Set (sRGB24 131 148 150) -- base0
    -- Set the extended palette that has 2 Vecs of 8 Solarized pallette colours
    , palette = ExtendedPalette solarizedDark1 solarizedDark2
    }
  where
    solarizedDark1 :: Vec N8 (Colour Double)
    solarizedDark1 =
         sRGB24   0  43  54 -- base03, background
      :* sRGB24 220  50  47 -- red
      :* sRGB24 133 153   0 -- green
      :* sRGB24 181 137   0 -- yellow
      :* sRGB24  38 139 210 -- blue
      :* sRGB24 211  54 130 -- magenta
      :* sRGB24  42 161 152 -- cyan
      :* sRGB24 238 232 213 -- base2
      :* EmptyVec

    solarizedDark2 :: Vec N8 (Colour Double)
    solarizedDark2 =
         sRGB24   7  54  66 -- base02, background highlights
      :* sRGB24 203  75  22 -- orange
      :* sRGB24  88 110 117 -- base01, comments / secondary text
      :* sRGB24 131 148 150 -- base0, body text / default code / primary content
      :* sRGB24 147 161 161 -- base1, optional emphasised content
      :* sRGB24 108 113 196 -- violet
      :* sRGB24 101 123 131 -- base00
      :* sRGB24 253 246 227 -- base3
      :* EmptyVec

-- This is our Solarized light 'ColourConfig'.  It holds all of our light-related settings.
solarizedLight :: ColourConfig (Colour Double)
solarizedLight =
  defaultColourConfig
    -- Set the default foreground colour of text of the terminal.
    { foregroundColour = Set (sRGB24 101 123 131) -- base00
    -- Set the extended palette that has 2 Vecs of 8 Solarized pallette colours
    , palette = ExtendedPalette solarizedLight1 solarizedLight2
    }
  where
    solarizedLight1 :: Vec N8 (Colour Double)
    solarizedLight1 =
         sRGB24 238 232 213 -- base2, background highlights
      :* sRGB24 220  50  47 -- red
      :* sRGB24 133 153   0 -- green
      :* sRGB24 181 137   0 -- yellow
      :* sRGB24  38 139 210 -- blue
      :* sRGB24 211  54 130 -- magenta
      :* sRGB24  42 161 152 -- cyan
      :* sRGB24   7  54  66 -- base02
      :* EmptyVec

    solarizedLight2 :: Vec N8 (Colour Double)
    solarizedLight2 =
         sRGB24 253 246 227 -- base3, background
      :* sRGB24 203  75  22 -- orange
      :* sRGB24 147 161 161 -- base1, comments / secondary text
      :* sRGB24 101 123 131 -- base00, body text / default code / primary content
      :* sRGB24  88 110 117 -- base01, optional emphasised content
      :* sRGB24 108 113 196 -- violet
      :* sRGB24 131 148 150 -- base0
      :* sRGB24   0  43  54 -- base03
      :* EmptyVec

main :: IO ()
main = do
  -- First, create the colour extension based on either Solarixed modules.
  myColourExt <- createColourExtension solarizedDark

  -- Update 'myTMConfig' with our colour extension.
  let newTMConfig = addColourExtension myTMConfig myColourExt

  -- Start Termonad with our updated 'TMConfig'.
  start newTMConfig
