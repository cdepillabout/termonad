-- | Module    : Termonad.IdMap
-- Description : A Map that keeps track of the ID of values
-- Copyright   : (c) Dennis Gosnell, 2023
-- License     : BSD3
-- Stability   : experimental
-- Portability : POSIX

module Termonad.IdMap
  ( IdMapKey
  , IdMap
  , emptyIdMap
  , singletonIdMap
  , insertIdMap
  , lookupIdMap
  , keysIdMap
  ) where

import Termonad.IdMap.Internal
