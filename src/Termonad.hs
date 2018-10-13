module Termonad
  ( defaultMain
  , start
  , module Config
  , module Extension
  ) where

import Termonad.App (defaultMain, start)
import Termonad.Config as Config
import Termonad.Config.Extension as Extension
