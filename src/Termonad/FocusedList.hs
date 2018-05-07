module Termonad.FocusedList where

import Termonad.Prelude

import Data.Finite (Finite)
import GHC.TypeLits (Nat)

-- TODO: Probably be better
-- implemented as an Order statistic tree
-- (https://en.wikipedia.org/wiki/Order_statistic_tree).
data FocusedList :: Nat -> * -> * where
  FocusedList :: KnownNat n => Finite n -> IntMap a -> FocusedList n a

singleton :: a -> FocusedList 1 a
singleton a = FocusedList (finite 0) (singletonMap 0 a)

append :: forall n. a -> FocusedList n a -> FocusedList (n + 1) a
append a (FocusedList fin intmap) =
  FocusedList (weaken fin) (insertMap (natVal @n) a intmap)

-- | This will return a 'FocusedList' with an empty element at index @m@.
unsafeShiftUpFrom ::
     (KnownNat m, m <= n) => proxy m -> FocusedList n a -> FocusedList (n + 1) a
unsafeShiftUpFrom _ (FocusedList fin intmap) =
  let newFin =
        if getFinite fin < natVal @m
          then weaken fin
          else shift fin
      newMap = unsafeShiftMapUp (natVal @n - 1) (natVal @m - 1) intmap
  in FocusedList newFin newMap

unsafeLookup :: Int -> IntMap a -> a
unsafeLookup i intmap =
  case lookup i intmap of
    Nothing -> error "unsafeLookup: key " <> show i <> " not found in intmap"
    Just a -> a

unsafeShiftMapUp :: Int -> Int -> IntMap -> IntMap
unsafeShiftMapUp start end intmap
  | start > end =
      let val = unsafeLookup start intmap
          newMap = insertMap (start + 1) val (delete start intmap)
      in unsafeShiftMapUp (start - 1) end newMap
  | otherwise = intmap

insert ::
     (KnownNat m, m <= n) => proxy m -> a -> FocusedList n a -> FocusedList (n + 1) a
insert proxyM a focusedList =
  let FocusedList fin intmap = unsafeShiftUpFrom proxyM focusedList
      newMap = insert (natVal @m) a intmap
  in FocusedList fin newMap
