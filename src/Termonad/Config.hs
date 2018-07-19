{-# LANGUAGE TemplateHaskell #-}

module Termonad.Config where

import Termonad.Prelude

import Control.Lens (makeLensesFor)

data FontConfig = FontConfig
  { fontFamily :: !Text
  , fontSize :: !Int
  } deriving (Eq, Show)

$(makeLensesFor
    [ ("fontFamily", "lensFontFamily")
    , ("fontSize", "lensFontSize")
    ]
    ''FontConfig
 )

defaultFontConfig :: FontConfig
defaultFontConfig =
  FontConfig
    { fontFamily = "Monospace" -- or "DejaVu Sans Mono" or "Bitstream Vera Sans Mono Roman" or "Source Code Pro"
    , fontSize = 12
    }

data ShowScrollbar
  = ShowScrollbarNever
  | ShowScrollbarAlways
  | ShowScrollbarIfNeeded
  deriving (Eq, Show)

data TMConfig = TMConfig
  { fontConfig :: !FontConfig
  , showScrollbar :: !ShowScrollbar
  } deriving (Eq, Show)

$(makeLensesFor
    [ ("fontConfig", "lensFontConfig")
    , ("showScrollbar", "lensShowScrollbar")
    ]
    ''TMConfig
 )

defaultTMConfig :: TMConfig
defaultTMConfig =
  TMConfig
    { fontConfig = defaultFontConfig
    , showScrollbar = ShowScrollbarIfNeeded
    }
