{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Termonad.FocusList where

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
data FocusList a = FocusList
  { focusListFocus :: {-# UNPACK #-} !Focus
  , focusListLen :: {-# UNPACK #-} !Int
  , focusList :: !(IntMap a)
  } deriving (Read, Show)

$(makeLensesFor
    [ ("focusListFocus", "focusListFocusLens")
    , ("focusListLen", "focusListLenLens")
    , ("focusList", "focusListLens")
    ]
    ''FocusList
 )

focusListAtLens :: Int -> Lens' (FocusList a) (Maybe a)
focusListAtLens i = focusListLens . at i

singletonFL :: a -> FocusList a
singletonFL a =
  FocusList
    { focusListFocus = 0
    , focusListLen = 1
    , focusList = singletonMap 0 a
    }

appendFL :: a -> FocusList a -> FocusList a
appendFL a fl =
  fl &
    focusListLenLens +~ 1 &
    focusListAtLens (fl ^. focusListLenLens) .~ Just a

insertFL :: Int -> FocusList a -> Maybe (FocusList a)
insertFL i fl = undefined

-- -- | This will return a 'FocusList' with an empty element at index @m@.
-- unsafeShiftUpFrom ::
--      forall proxy m n a. (KnownNat m, KnownNat n, m <= n, 1 <= n)
--   => proxy m
--   -> FocusList n a
--   -> FocusList (n + 1) a
-- unsafeShiftUpFrom _ (FocusList fin intmap) =
--   let newFin =
--         if getFiniteInt fin < natValInt @m
--           then weaken fin
--           else shift fin
--       newMap = unsafeShiftMapUp (natValInt @n - 1) (natValInt @m - 1) intmap
--   in
--   case plusNat @n @1 of
--     Sub Dict -> FocusList newFin newMap

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
--   -> FocusList n a
--   -> FocusList (n + 1) a
-- insertFL _ a FocusListEmpty = singletonFL a
-- insertFL proxyM a focusList@(FocusList fin _) =
--   case lala fin of
--     Refl ->
--       case unsafeShiftUpFrom proxyM focusList of
--         FocusListEmpty -> absurd $ gaga (Proxy @n)
--         FocusList newFin intmap ->
--           let newMap = insertMap (natValInt @m) a intmap
--           in
--           case plusNat @n @1 of
--             Sub Dict -> FocusList newFin newMap

-- natValInt :: forall m. KnownNat m => Int
-- natValInt = fromIntegral $ natVal (Proxy @m)

-- getFiniteInt :: Finite n -> Int
-- getFiniteInt = fromIntegral . getFinite
