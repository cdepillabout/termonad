{-# LANGUAGE TemplateHaskell #-}

module Termonad.Config where

import Termonad.Prelude

import Control.Lens (makeLensesFor)
import Data.Colour (Colour)
import Data.Colour.Names -- (grey)

data FontConfig = FontConfig
  { fontFamily :: !Text
  , fontSize :: !Int
  , fontInPixels :: !Bool
  } deriving (Eq, Show)

$(makeLensesFor
    [ ("fontFamily", "lensFontFamily")
    , ("fontSize", "lensFontSize")
    , ("fontInPixels", "lensFontInPixels")
    ]
    ''FontConfig
 )

defaultFontConfig :: FontConfig
defaultFontConfig =
  FontConfig
    { fontFamily = "Monospace" -- or "DejaVu Sans Mono" or "Bitstream Vera Sans Mono Roman" or "Source Code Pro"
    , fontSize = 12
    , fontInPixels = False
    }

data ShowScrollbar
  = ShowScrollbarNever
  | ShowScrollbarAlways
  | ShowScrollbarIfNeeded
  deriving (Eq, Show)

data TMConfig = TMConfig
  { fontConfig :: !FontConfig
  , showScrollbar :: !ShowScrollbar
  , cursorColor :: !(Colour Double)
  , scrollbackLen :: !Integer
  , confirmExit :: !Bool
  , wordCharExceptions :: !Text
  } deriving (Eq, Show)

$(makeLensesFor
    [ ("fontConfig", "lensFontConfig")
    , ("showScrollbar", "lensShowScrollbar")
    , ("cursorColor", "lensCursorColor")
    , ("scrollbackLen", "lensScrollbackLen")
    , ("confirmExit", "lensConfirmExit")
    , ("wordCharExceptions", "lensWordCharExceptions")
    ]
    ''TMConfig
 )

defaultTMConfig :: TMConfig
defaultTMConfig =
  TMConfig
    { fontConfig = defaultFontConfig
    , showScrollbar = ShowScrollbarIfNeeded
    , cursorColor = lightgrey
    , scrollbackLen = 10000
    , confirmExit = True
    , wordCharExceptions = "-#%&+,./=?@\\_~\183:"
    }
