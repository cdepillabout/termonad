-- | Module    : Termonad
-- Description : Termonad
-- Copyright   : (c) Dennis Gosnell, 2018
-- License     : BSD3
-- Stability   : experimental
-- Portability : POSIX
--
-- This module exposes termonad's basic configuration options, as well as 'defaultMain', 'startWithCliArgs', and 'start'.
--
-- If you want to configure Termonad, please take a look at "Termonad.Config".

module Termonad
  ( defaultMain
  , startWithCliArgs
  , start
  , module Config
  ) where

import Termonad.App (defaultMain, start, startWithCliArgs)
import Termonad.Config as Config
