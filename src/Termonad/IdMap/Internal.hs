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

newtype IdMapKey = IdMapKey { unIdMapKey :: Int }
  deriving stock (Eq, Show)

data IdMap a = IdMap
  { idMap :: !(IntMap a)
  , nextId :: !Int
  }
  deriving stock Show

-- | 'IdMap's are equal if they contain the same elements at the same keys.
--
-- >>> let (helloKey, idmapA) = insertIdMap "hello" emptyIdMap
-- >>> let (_, idmapB) = singletonIdMap "hello"
-- >>> idmapA == idmapB
-- True
--
-- Note that if you delete and reinsert a value, it will get a different key,
-- so will no longer be equal.
--
-- >>> let (_, idmapA') = insertIdMap "hello" $ deleteIdMap helloKey idmapA
-- >>> idmapA' == idmapB
-- False
--
-- However, 'IdMap's don't check the 'nextId' field when determining equality.
--
-- >>> let (byeKey, idmapA'') = insertIdMap "bye" idmapA
-- >>> let idmapA''' = deleteIdMap byeKey idmapA''
-- >>> idmapA''' == idmapB
-- True
instance Eq a => Eq (IdMap a) where
  (IdMap idMapA _) == (IdMap idMapB _) = idMapA == idMapB

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

-- | Get the next available ID.
--
-- >>> succId 3
-- 4
succId :: Int -> Int
succId i = i + 1

-- | An initial 'IdMap' with no values.
--
-- >>> emptyIdMap
-- IdMap {idMap = fromList [], nextId = 0}
emptyIdMap :: IdMap a
emptyIdMap = IdMap { idMap = mempty, nextId = 0 }

-- | Insert a value into an 'IdMap'.  Returns the key for the newly inserted
-- item.
--
-- >>> let (key, idmap) = insertIdMap "hello" emptyIdMap
-- >>> (key, idmap)
-- (IdMapKey {unIdMapKey = 0},IdMap {idMap = fromList [(0,"hello")], nextId = 1})
--
-- >>> insertIdMap "zoom" idmap
-- (IdMapKey {unIdMapKey = 1},IdMap {idMap = fromList [(0,"hello"),(1,"zoom")], nextId = 2})
insertIdMap :: a -> IdMap a -> (IdMapKey, IdMap a)
insertIdMap a IdMap {idMap, nextId} =
  let newMap = IntMap.insert nextId a idMap
      newNextId = nextId + 1
  in (IdMapKey nextId, IdMap { idMap = newMap, nextId = newNextId })

-- | Create an 'IdMap' with a single value.
--
-- >>> singletonIdMap "hello"
-- (IdMapKey {unIdMapKey = 0},IdMap {idMap = fromList [(0,"hello")], nextId = 1})
--
-- prop> \a -> insertIdMap a emptyIdMap == singletonIdMap a
singletonIdMap :: a -> (IdMapKey, IdMap a)
singletonIdMap a = insertIdMap a emptyIdMap

-- | Lookup the given key in an 'IdMap'.
--
-- >>> let (key, idmap) = insertIdMap "hello" emptyIdMap
-- >>> lookupIdMap key idmap
-- Just "hello"
--
-- Trying to lookup keys that don't exist returns 'Nothing':
--
-- >>> let idmap' = deleteIdMap key idmap
-- >>> lookupIdMap key idmap'
-- Nothing
lookupIdMap :: IdMapKey -> IdMap a -> Maybe a
lookupIdMap (IdMapKey k) IdMap {idMap} = IntMap.lookup k idMap

-- | List all keys in an 'IdMap'.
--
-- >>> let (_, idmap) = singletonIdMap "hello"
-- >>> let (_, idmap') = insertIdMap "bye" idmap
-- >>> keysIdMap idmap'
-- [IdMapKey {unIdMapKey = 0},IdMapKey {unIdMapKey = 1}]
--
-- Returns the empty list when passed an empty 'IdMap':
--
-- >>> keysIdMap emptyIdMap
-- []
keysIdMap :: IdMap a -> [IdMapKey]
keysIdMap IdMap {idMap} = fmap IdMapKey $ IntMap.keys idMap

-- | Delete a key and its value from the map. When the key is not a member of
-- the map, the original map is returned.
--
-- >>> let (key, idmap) = singletonIdMap "hello"
-- >>> let (_, idmap') = insertIdMap "bye" idmap
-- >>> deleteIdMap key idmap'
-- IdMap {idMap = fromList [(1,"bye")], nextId = 2}
--
-- Deleting a key that does not exist just returns the old map:
--
-- >>> deleteIdMap key idmap'
-- IdMap {idMap = fromList [(1,"bye")], nextId = 2}
deleteIdMap :: IdMapKey -> IdMap a -> IdMap a
deleteIdMap (IdMapKey k) IdMap {idMap, nextId} =
  IdMap
    { idMap = IntMap.delete k idMap
    , nextId
    }
