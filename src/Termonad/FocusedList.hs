{-# LANGUAGE AllowAmbiguousTypes #-}

module Termonad.FocusedList where

import Termonad.Prelude

import Data.Constraint ((:-)(Sub), Dict(Dict))
import Data.Constraint.Nat (plusNat)
import Data.Finite (Finite, finite, getFinite, shift, weaken)
import GHC.TypeLits (type (+), type (<=), KnownNat, Nat, natVal)

-- TODO: Probably be better
-- implemented as an Order statistic tree
-- (https://en.wikipedia.org/wiki/Order_statistic_tree).
data FocusedList :: Nat -> * -> * where
  FocusedList
    :: KnownNat n
    => Finite n
    -> IntMap a
    -> FocusedList n a

singletonFL :: a -> FocusedList 1 a
singletonFL a = FocusedList (finite 0) (singletonMap 0 a)

appendFL :: forall n a. a -> FocusedList n a -> FocusedList (n + 1) a
appendFL a (FocusedList fin intmap) =
  case plusNat @n @1 of
    Sub Dict ->
      FocusedList (weaken fin) (insertMap (natValTA @n) a intmap)

-- | This will return a 'FocusedList' with an empty element at index @m@.
unsafeShiftUpFrom ::
     forall proxy m n a. (KnownNat m, m <= n)
  => proxy m
  -> FocusedList n a
  -> FocusedList (n + 1) a
unsafeShiftUpFrom _ (FocusedList fin intmap) =
  let newFin =
        if getFiniteInt fin < natValTA @m
          then weaken fin
          else shift fin
      newMap = unsafeShiftMapUp (natValTA @n - 1) (natValTA @m - 1) intmap
  in
  case plusNat @n @1 of
    Sub Dict -> FocusedList newFin newMap

unsafeLookup :: Int -> IntMap a -> a
unsafeLookup i intmap =
  case lookup i intmap of
    Nothing -> error $ "unsafeLookup: key " <> show i <> " not found in intmap"
    Just a -> a

unsafeShiftMapUp :: Int -> Int -> IntMap a -> IntMap a
unsafeShiftMapUp start end intmap
  | start > end =
      let val = unsafeLookup start intmap
          newMap = insertMap (start + 1) val (deleteMap start intmap)
      in unsafeShiftMapUp (start - 1) end newMap
  | otherwise = intmap

insertFL ::
     forall proxy m n a. (KnownNat m, m <= n)
  => proxy m
  -> a
  -> FocusedList n a
  -> FocusedList (n + 1) a
insertFL proxyM a focusedList@FocusedList{} =
  let FocusedList fin intmap = unsafeShiftUpFrom proxyM focusedList
      newMap = insertMap (natValTA @m) a intmap
  in
  case plusNat @n @1 of
    Sub Dict -> FocusedList fin newMap

natValTA :: forall m. KnownNat m => Int
natValTA = fromIntegral $ natVal (Proxy @m)

getFiniteInt :: Finite n -> Int
getFiniteInt = fromIntegral . getFinite
