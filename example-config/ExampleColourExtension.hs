-- | This is an example Termonad configuration that shows how to use the
-- 'ColourExtension' to set colours for your terminal.  See the project-wide
-- README.md for how to use Termonad Haskell configuration scripts.

module Main where

import Termonad
  ( CursorBlinkMode(CursorBlinkModeOff), Option(Set)
  , ShowScrollbar(ShowScrollbarNever), TMConfig, confirmExit, cursorBlinkMode
  , defaultConfigOptions, defaultTMConfig, options, showMenu, showScrollbar
  , start
  )
import Termonad.Config.Colour
  ( Colour, ColourConfig, Palette(BasicPalette), addColourExtension
  , createColourExtension, cursorBgColour, defaultColourConfig, foregroundColour
  , palette, sRGB24
  )
import Termonad.Config.Vec (Vec((:*), EmptyVec), N8)
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

-- This is our 'ColourConfig'.  It holds all of our colour-related settings.
myColourConfig :: ColourConfig (Colour Double)
myColourConfig =
  defaultColourConfig
    -- Set the cursor background colour.  This is the normal colour of the
    -- cursor.
    { cursorBgColour = Set (sRGB24 120 80 110) -- purple
    -- Set the default foreground colour of text of the terminal.
    , foregroundColour = sRGB24 220 180 210 -- light pink
    -- Set the basic palette that has 8 colours.
    , palette = BasicPalette myStandardColours
    }
  where
    -- This is a length-indexed linked-list of colours.
    myStandardColours :: Vec N8 (Colour Double)
    myStandardColours =
         sRGB24  40  30  20 -- dark brown (used as background colour)
      :* sRGB24 180  30  20 -- red
      :* sRGB24  40 160  20 -- green
      :* sRGB24 180 160  20 -- dark yellow
      :* sRGB24  40  30 120 -- dark purple
      :* sRGB24 180  30 120 -- bright pink
      :* sRGB24  40 160 120 -- teal
      :* sRGB24 180 160 120 -- light brown
      :* EmptyVec

main :: IO ()
main = do
  -- First, create the colour extension based on 'myColourConfig'.
  myColourExt <- createColourExtension myColourConfig

  -- Update 'myTMConfig' with our colour extension.
  let newTMConfig = addColourExtension myTMConfig myColourExt

  -- Start Termonad with our updated 'TMConfig'.
  start newTMConfig
