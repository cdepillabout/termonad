-- | Module    : Termonad
-- Description : Termonad
-- Copyright   : (c) Dennis Gosnell, 2018
-- License     : BSD3
-- Stability   : experimental
-- Portability : POSIX
--
-- This module exposes Termonad's basic configuration options, as well as
-- 'defaultMain', 'startWithCliArgs', 'start', and 'startWith'.
--
-- If you want to configure Termonad, please take a look at "Termonad.Config".

module Termonad
  ( defaultMain
  , startWithCliArgs
  , start
  , startWith
  , InstanceMode(..)
  , module Config
  ) where

import Termonad.App (start, startWith)
import Termonad.Config as Config
import Termonad.Startup (defaultMain, startWithCliArgs)
import Termonad.Types (InstanceMode(..))
