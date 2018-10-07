module Termonad
  ( defaultMain
  , pattern (:+)
  , CursorBlinkMode(..)
  , module Config
  ) where

import Termonad.App (defaultMain)
import Termonad.Config as Config

import Data.Type.Vector (pattern (:+))
import GI.Vte (CursorBlinkMode(..))
