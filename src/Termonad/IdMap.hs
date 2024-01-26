-- | Module    : Termonad.IdMap
-- Description : A Map that keeps track of the ID of values
-- Copyright   : (c) Dennis Gosnell, 2023
-- License     : BSD3
-- Stability   : experimental
-- Portability : POSIX
--
-- An 'IdMap' is a combination between an 'IntMap' and a 'Set'.
--
-- An 'IdMap' allows adding an arbitrary number of things to be tracked.  It
-- returns an 'IdMapKey' whenever a new item is added to the set.  This
-- 'IdMapKey' can then be used to lookup items already in the set.

module Termonad.IdMap
  ( IdMapKey
  , IdMap
  , emptyIdMap
  , singletonIdMap
  , insertIdMap
  , lookupIdMap
  , keysIdMap
  , deleteIdMap
  ) where

import Termonad.IdMap.Internal
