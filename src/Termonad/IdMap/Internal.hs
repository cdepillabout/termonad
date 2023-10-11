-- | Module    : Termonad.IdMap
-- Description : A Map that keeps track of the ID of values
-- Copyright   : (c) Dennis Gosnell, 2023
-- License     : BSD3
-- Stability   : experimental
-- Portability : POSIX

module Termonad.IdMap.Internal where

import Termonad.Prelude

import Control.Lens (FoldableWithIndex, ifoldMap, Index, IxValue, Traversal', Ixed (ix))
import qualified Data.Foldable as Foldable
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

-- TODO: Write tests for this!

newtype IdMapKey = IdMapKey { unIdMapKey :: Int }
  deriving stock Show

data IdMap a = IdMap
  { idMap :: !(IntMap a)
  , nextId :: !Int
  }
  deriving stock Show

instance Functor IdMap where
  fmap f IdMap{idMap, nextId} = IdMap { idMap = fmap f idMap, nextId }

instance Foldable IdMap where
  foldMap f m = Foldable.foldMap f $ idMap m

instance FoldableWithIndex Int IdMap where
  ifoldMap f m = ifoldMap f $ idMap m

instance Traversable IdMap where
  traverse f IdMap{idMap, nextId} =
    fmap (\m -> IdMap { idMap = m, nextId }) (traverse f idMap)

type instance Index (IdMap a) = IdMapKey
type instance IxValue (IdMap a) = a

instance Ixed (IdMap a) where
  ix :: IdMapKey -> Traversal' (IdMap a) a
  ix (IdMapKey i) f IdMap{idMap, nextId} =
    case IntMap.lookup i idMap of
      Just v -> fmap update (f v) -- f v <&> \v' -> IntMap.insert k v' m
      Nothing -> pure IdMap{idMap, nextId}
    where
      update :: a -> IdMap a
      update v' =
        IdMap
          { idMap = IntMap.adjust (const v') i idMap
          , nextId
          }

initialId :: Int
initialId = 0

succId :: Int -> Int
succId i = i + 1

emptyIdMap :: IdMap a
emptyIdMap = IdMap { idMap = mempty, nextId = 0 }

insertIdMap :: a -> IdMap a -> (IdMapKey, IdMap a)
insertIdMap a IdMap {idMap, nextId} =
  let newMap = IntMap.insert nextId a idMap
      newNextId = nextId + 1
  in (IdMapKey nextId, IdMap { idMap = newMap, nextId = newNextId })

singletonIdMap :: a -> (IdMapKey, IdMap a)
singletonIdMap a = insertIdMap a emptyIdMap

lookupIdMap :: IdMapKey -> IdMap a -> Maybe a
lookupIdMap (IdMapKey k) IdMap {idMap} = IntMap.lookup k idMap

keysIdMap :: IdMap a -> [IdMapKey]
keysIdMap IdMap {idMap} = fmap IdMapKey $ IntMap.keys idMap
