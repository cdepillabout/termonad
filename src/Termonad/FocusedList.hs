{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Termonad.FocusedList where

import Termonad.Prelude

import Control.Lens
import Control.Lens.TH
import Data.Constraint ((:-)(Sub), Dict(Dict))
import Data.Constraint.Nat (plusNat)
import Data.Finite (Finite, finite, getFinite, shift, weaken)
import Data.Type.Equality ((:~:)(Refl))
import Data.Void
import GHC.TypeLits (type (+), type (<=), type (<=?), KnownNat, Nat, natVal)
import Unsafe.Coerce (unsafeCoerce)

newtype Focus = Focus
  { unFocus :: Int
  } deriving newtype (Eq, Num, Read, Show)

-- TODO: Probably be better
-- implemented as an Order statistic tree
-- (https://en.wikipedia.org/wiki/Order_statistic_tree).
data FocusedList a = FocusedList
  { focusedListFocus :: {-# UNPACK #-} !Focus
  , focusedListLen :: {-# UNPACK #-} !Int
  , focusedList :: !(IntMap a)
  } deriving (Read, Show)

$(makeLensesFor
    [ ("focusedListFocus", "focusedListFocusLens")
    , ("focusedListLen", "focusedListLenLens")
    , ("focusedList", "focusedListLens")
    ]
    ''FocusedList
 )

focusedListAtLens :: Int -> Lens' (FocusedList a) (Maybe a)
focusedListAtLens i = focusedListLens . at i

singletonFL :: a -> FocusedList a
singletonFL a =
  FocusedList
    { focusedListFocus = 0
    , focusedListLen = 1
    , focusedList = singletonMap 0 a
    }

appendFL :: a -> FocusedList a -> FocusedList a
appendFL a fl =
  fl &
    focusedListLenLens +~ 1 &
    focusedListAtLens (fl ^. focusedListLenLens) .~ Just a

insertFL :: Int -> FocusedList a -> Maybe (FocusedList a)
insertFL i fl = undefined

-- -- | This will return a 'FocusedList' with an empty element at index @m@.
-- unsafeShiftUpFrom ::
--      forall proxy m n a. (KnownNat m, KnownNat n, m <= n, 1 <= n)
--   => proxy m
--   -> FocusedList n a
--   -> FocusedList (n + 1) a
-- unsafeShiftUpFrom _ (FocusedList fin intmap) =
--   let newFin =
--         if getFiniteInt fin < natValInt @m
--           then weaken fin
--           else shift fin
--       newMap = unsafeShiftMapUp (natValInt @n - 1) (natValInt @m - 1) intmap
--   in
--   case plusNat @n @1 of
--     Sub Dict -> FocusedList newFin newMap

-- unsafeLookup :: Int -> IntMap a -> a
-- unsafeLookup i intmap =
--   case lookup i intmap of
--     Nothing -> error $ "unsafeLookup: key " <> show i <> " not found in intmap"
--     Just a -> a

-- unsafeShiftMapUp :: Int -> Int -> IntMap a -> IntMap a
-- unsafeShiftMapUp start end intmap
--   | start > end =
--       let val = unsafeLookup start intmap
--           newMap = insertMap (start + 1) val (deleteMap start intmap)
--       in unsafeShiftMapUp (start - 1) end newMap
--   | otherwise = intmap

-- insertFL ::
--      forall proxy m n a. (KnownNat m, KnownNat n, m <= n)
--   => proxy m
--   -> a
--   -> FocusedList n a
--   -> FocusedList (n + 1) a
-- insertFL _ a FocusedListEmpty = singletonFL a
-- insertFL proxyM a focusedList@(FocusedList fin _) =
--   case lala fin of
--     Refl ->
--       case unsafeShiftUpFrom proxyM focusedList of
--         FocusedListEmpty -> absurd $ gaga (Proxy @n)
--         FocusedList newFin intmap ->
--           let newMap = insertMap (natValInt @m) a intmap
--           in
--           case plusNat @n @1 of
--             Sub Dict -> FocusedList newFin newMap

-- natValInt :: forall m. KnownNat m => Int
-- natValInt = fromIntegral $ natVal (Proxy @m)

-- getFiniteInt :: Finite n -> Int
-- getFiniteInt = fromIntegral . getFinite
