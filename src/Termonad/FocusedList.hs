{-# LANGUAGE AllowAmbiguousTypes #-}

module Termonad.FocusedList where

import Termonad.Prelude

import Data.Constraint ((:-)(Sub), Dict(Dict))
import Data.Constraint.Nat (plusNat)
import Data.Finite (Finite, finite, getFinite, shift, weaken)
import Data.Type.Equality ((:~:)(Refl))
import Data.Void
import GHC.TypeLits (type (+), type (<=), type (<=?), KnownNat, Nat, natVal)
import Unsafe.Coerce (unsafeCoerce)


-- TODO: Reimplement this stuff without using KnownNat.

-- TODO: Probably be better
-- implemented as an Order statistic tree
-- (https://en.wikipedia.org/wiki/Order_statistic_tree).
data FocusedList :: Nat -> * -> * where
  FocusedListEmpty :: FocusedList 0 a
  FocusedList :: Finite n -> IntMap a -> FocusedList n a

singletonFL :: a -> FocusedList 1 a
singletonFL a = FocusedList (finite 0) (singletonMap 0 a)

appendFL :: forall n a. KnownNat n => a -> FocusedList n a -> FocusedList (n + 1) a
appendFL a FocusedListEmpty = singletonFL a
appendFL a (FocusedList fin intmap) =
  case plusNat @n @1 of
    Sub Dict ->
      FocusedList (weaken fin) (insertMap (natValInt @n) a intmap)

-- | This will return a 'FocusedList' with an empty element at index @m@.
unsafeShiftUpFrom ::
     forall proxy m n a. (KnownNat m, KnownNat n, m <= n, 1 <= n)
  => proxy m
  -> FocusedList n a
  -> FocusedList (n + 1) a
unsafeShiftUpFrom _ (FocusedList fin intmap) =
  let newFin =
        if getFiniteInt fin < natValInt @m
          then weaken fin
          else shift fin
      newMap = unsafeShiftMapUp (natValInt @n - 1) (natValInt @m - 1) intmap
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
     forall proxy m n a. (KnownNat m, KnownNat n, m <= n)
  => proxy m
  -> a
  -> FocusedList n a
  -> FocusedList (n + 1) a
insertFL _ a FocusedListEmpty = singletonFL a
insertFL proxyM a focusedList@(FocusedList fin _) =
  case lala fin of
    Refl ->
      case unsafeShiftUpFrom proxyM focusedList of
        FocusedListEmpty -> absurd $ gaga (Proxy @n)
        FocusedList newFin intmap ->
          let newMap = insertMap (natValInt @m) a intmap
          in
          case plusNat @n @1 of
            Sub Dict -> FocusedList newFin newMap

papa :: Int
papa =
  let a = singletonFL ("hello" :: Text)
      b = insertFL (Proxy @1) "bye" a
      c = insertFL (Proxy @2) "good" b
      d = insertFL (Proxy @0) "yo" c
  in
  case d of
    FocusedList _ _ -> 3

dada :: (1 <= n) => FocusedList n a -> Int
dada (FocusedList _ _) = 3

hoho :: (KnownNat n, 1 <= n, IsString a) => FocusedList n a -> Int
hoho fl =
  let a = insertFL (Proxy @0) "bye" fl
  in
  case a of
    FocusedList _ _ -> 10

gaga :: forall n proxy. (KnownNat n, (n + 1) ~ 0) => proxy n -> Void
gaga _ = error "void"

lala :: KnownNat x => Finite x -> (1 <=? x) :~: 'True
lala _ = unsafeCoerce Refl

natValInt :: forall m. KnownNat m => Int
natValInt = fromIntegral $ natVal (Proxy @m)

getFiniteInt :: Finite n -> Int
getFiniteInt = fromIntegral . getFinite
