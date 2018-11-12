-- | Module    : Termonad
-- Description : Termonad
-- Copyright   : (c) Dennis Gosnell, 2018
-- License     : BSD3
-- Stability   : experimental
-- Portability : POSIX
--
-- This module exposes termonad's basic configuration options, as well as 'defaultMain'.
--
-- If you want to configure Termonad, please take a look at "Termonad.Config".

module Termonad
  ( defaultMain
  , start
  , module Config
  ) where

import Termonad.App (defaultMain, start)
import Termonad.Config as Config
