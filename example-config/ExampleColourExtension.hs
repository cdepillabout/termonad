-- | This is an example Termonad configuration that shows how to use the
-- 'ColourExtension' to set colours for your terminal.  See the project-wide
-- README.md for how to use Termonad Haskell configuration scripts.

module Main where

import Data.Colour.SRGB (Colour, sRGB24)
import Data.Singletons (sing)
import Termonad
  ( CursorBlinkMode(CursorBlinkModeOff), DefaultOrUser(User), Option(Set)
  , ShowScrollbar(ShowScrollbarNever), TMConfig, confirmExit, cursorBlinkMode
  , defaultConfigOptions, defaultTMConfig, options, showMenu, showScrollbar
  , start
  )
import Termonad.Config.Colour
  ( Colour, ColourConfig, Palette(ExtendedPalette), addColourExtension
  , createColourExtension, cursorBgColour, defaultColourConfig, foregroundColour
  , palette, sRGB24
  )
import Termonad.Config.Vec
  ( N4, N8, Sing, Vec((:*), EmptyVec), fin_, setAtVec, unsafeFromListVec_
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
    , foregroundColour = User (sRGB24 220 180 210) -- light pink
    -- Set the extended palette that has 8 colours standard colors and then 8
    -- light colors.
    , palette = ExtendedPalette myStandardColours myLightColours
    }
  where
    -- This is a an example of creating a length-indexed linked-list of colours,
    -- using 'Vec' constructors.
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

    -- This is an example of creating a length-indexed linked-list of colours,
    -- using the 'unsafeFromListVec_' function.  'unsafeFromListVec_' is okay to
    -- use as long as you're absolutely sure you have 8 elements.
    myLightColours :: Vec N8 (Colour Double)
    myLightColours =
      unsafeFromListVec_
        [ sRGB24  70  60  50 -- brown
        , sRGB24 220  30  20 -- light red
        , sRGB24  40 210  20 -- light green
        , sRGB24 220 200  20 -- yellow
        , sRGB24  40  30 180 -- purple
        , sRGB24 140  30 80  -- dark pink
        , sRGB24  50 200 160 -- light teal
        , sRGB24 220 200 150 -- light brown
        ]

    -- This is an example of updating just a single value in a 'Colour' 'Vec'.
    -- Here we are updating the 5th 'Colour' (which is at index 4).
    updateSingleColor :: Vec N8 (Colour Double)
    updateSingleColor =
      let fin4 = fin_ (sing :: Sing N4)
      in setAtVec fin4 (sRGB24 40 30 150) myStandardColours

main :: IO ()
main = do
  -- First, create the colour extension based on 'myColourConfig'.
  myColourExt <- createColourExtension myColourConfig

  -- Update 'myTMConfig' with our colour extension.
  let newTMConfig = addColourExtension myTMConfig myColourExt

  -- Start Termonad with our updated 'TMConfig'.
  start newTMConfig
