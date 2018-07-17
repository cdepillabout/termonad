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

data TMConfig = TMConfig
  { fontConfig :: !FontConfig
  } deriving (Eq, Show)

$(makeLensesFor
    [ ("fontConfig", "lensFontConfig")
    ]
    ''TMConfig
 )

defaultTMConfig :: TMConfig
defaultTMConfig =
  TMConfig
    { fontConfig = defaultFontConfig
    }
