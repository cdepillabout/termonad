
module Main where

import Termonad
import Termonad.Config.Colour
import Termonad.Config.Vec (Vec((:*), EmptyVec), N8)
import Data.Colour.SRGB (Colour, sRGB24)

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

myColourConfig :: ColourConfig (Colour Double)
myColourConfig = defaultColourConfig
  { cursorBgColour = Set (sRGB24 120 80 110) -- purple
  , foregroundColour = sRGB24 220 180 210 -- light pink
  , palette = BasicPalette myStandardColours
  } where
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
  myColourExt <- createColourExtension myColourConfig
  start (addColourExtension myTMConfig myColourExt)
