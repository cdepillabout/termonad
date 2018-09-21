module Termonad
  ( defaultMain
  , pattern (:+)
  , module Config
  ) where

import Termonad.App (defaultMain)
import Termonad.Config as Config

import Data.Type.Vector (pattern (:+))
