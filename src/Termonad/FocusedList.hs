{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Termonad.FocusList where

import Termonad.Prelude

import Control.Lens

data Focus = Focus Int | NoFocus deriving (Eq, Read, Show)

-- TODO: Probably be better
-- implemented as an Order statistic tree
-- (https://en.wikipedia.org/wiki/Order_statistic_tree).
data FocusList a = FocusList
  { focusListFocus :: {-# UNPACK #-} !Focus
  , focusListLen :: {-# UNPACK #-} !Int
  , focusList :: !(IntMap a)
  } deriving (Read, Show)

$(makeLensesFor
    [ ("focusListFocus", "lensFocusListFocus")
    , ("focusListLen", "lensFocusListLen")
    , ("focusList", "lensFocusList")
    ]
    ''FocusList
 )

lensFocusListAt :: Int -> Lens' (FocusList a) (Maybe a)
lensFocusListAt i = lensFocusList . at i

-- | This is an invariant that the 'FocusList' must always protect.
invariantFL :: FocusList a -> Bool
invariantFL fl = False

singletonFL :: a -> FocusList a
singletonFL a =
  FocusList
    { focusListFocus = Focus 1
    , focusListLen = 1
    , focusList = singletonMap 0 a
    }

-- | Return 'True' if the 'FocusList' is empty.
isEmptyFL :: FocusList a -> Bool
isEmptyFL fl = fl ^. lensFocusListLen == 0

-- | Append a value to the end of a 'FocusList'.
--
-- This can be thought of as a \"snoc\" operation.
appendFL :: FocusList a -> a -> FocusList a
appendFL fl a =
  if isEmptyFL fl
    then singletonFL a
    else unsafeInsertNewFL (fl ^. lensFocusListLen) a fl

-- | Prepend a value to a 'FocusList'.
--
-- This can be thought of as a \"cons\" operation.
prependFL :: a -> FocusList a -> FocusList a
prependFL a fl =
  if isEmptyFL fl
    then singletonFL a
    else unsafeInsertNewFL 0 a $ unsafeShiftUpFrom 0 fl

-- | Unsafely get the value in a 'Focus' from a 'FocusList'.  If the 'Focus' is
-- 'NoFocus', this function returns 'error'.
unsafeGetFocus :: FocusList a -> Int
unsafeGetFocus fl =
  let focus = fl ^. lensFocusListFocus
  in
  case focus of
    NoFocus ->
      error $
        "unsafeGetFocus: the focus list doesn't have a focus: " <> show focus
    Focus i -> i

-- | Unsafely insert a new @a@ in a 'FocusList'.  This sets the 'Int' value to
-- @a@.  The length of the 'FocusList' will be increased by 1.  The
-- 'FocusList's 'Focus' is not changed.
--
-- If there is some value in the 'FocusList' already at the 'Int', then it will
-- be overwritten.  Also, the 'Int' is not checked to make sure it is above 0.
unsafeInsertNewFL :: Int -> a -> FocusList a -> FocusList a
unsafeInsertNewFL i a fl =
  fl &
    lensFocusListLen +~ 1 &
    lensFocusListAt i ?~ a

-- | This unsafely shifts all values up in a 'FocusList'.  It also updates the
-- 'Focus' of the 'FocusList' if it has been shifted.  This does not change
-- the length of the 'FocusList'.
--
-- It does not check that the 'Int' is greater than 0.  It also does not check
-- that there is a 'Focus'.
unsafeShiftUpFrom :: forall a. Int -> FocusList a -> FocusList a
unsafeShiftUpFrom i fl =
  let intMap = fl ^. lensFocusList
      lastElemIdx = (fl ^. lensFocusListLen) - 1
      newIntMap = go i lastElemIdx intMap
      oldFocus = unsafeGetFocus fl
      newFocus =
        if i > lastElemIdx
          then oldFocus
          else oldFocus + 1
  in
  fl &
    lensFocusList .~ newIntMap &
    lensFocusListFocus .~ Focus newFocus
  where
    go :: Int -> Int -> IntMap a -> IntMap a
    go idxToInsert idxToShiftUp intMap
      | idxToInsert <= idxToShiftUp =
        let val = unsafeLookup idxToShiftUp intMap
            newMap =
              insertMap (idxToShiftUp + 1) val (deleteMap idxToShiftUp intMap)
        in go idxToInsert (idxToShiftUp - 1) newMap
      | otherwise = intMap

-- | This is an unsafe lookup function.  This assumes that the 'Int' exists in
-- the 'IntMap'.
unsafeLookup :: Int -> IntMap a -> a
unsafeLookup i intmap =
  case lookup i intmap of
    Nothing -> error $ "unsafeLookup: key " <> show i <> " not found in intmap"
    Just a -> a

-- | Insert a new value into the 'FocusList'.  The 'Focus' of the list is
-- changed appropriately.
--
-- This returns 'Nothing' if the index at which to insert the new value is
-- either less than 0 or greater than the length of the list.
insertFL
  :: Int  -- ^ The index at which to insert the value.
  -> a
  -> FocusList a
  -> Maybe (FocusList a)
insertFL i a fl =
  if i < 0 || i > (fl ^. lensFocusListLen)
    then
      -- Return Nothing if the insertion position is out of bounds.
      Nothing
    else
      -- Otherwise, shift all existing values up one and insert the new
      -- value in the opened place.
      let shiftedUpFL = unsafeShiftUpFrom i fl
      in Just $ unsafeInsertNewFL i a shiftedUpFL

-- | Remove an element from a 'FocusList'.
--
-- If the element to remove is not the 'Focus', then update the 'Focus'
-- accordingly.  (For example, if the 'Focus' is on index 3, and we have
-- removed index 5, then the focus is not affected, so it is not changed.  If
-- the 'Focus' is on index 9 and we have removed index 6, then the 'Focus' will
-- be set to 8.)
--
-- If the element to remove is the only element in the list, then the 'Focus'
-- will be set to 'NoFocus'.
--
-- If the element to remove is the 'Focus', then use the function passed in to
-- compute the new 'Focus'.  This lets the use decide which element should get
-- the new focus.  Keep in mind that if the old 'Focus' was index 8, and the
-- function returns the new 'Focus' as index 8, then effectively the element
-- AFTER the element that was removed will have the focus.
--
-- If the 'Int' for the index to remove is either less than 0 or greater then
-- the length of the list, then 'Nothing' is returned.  If the 'FocusList'
-- passed in is 'Empty', then 'Nothing' is returned.  If the 'Int' returned
-- by the update function is less than 0 or greater than the length of the new
-- list, then 'Nothing' is returned.
removeFL
  :: Int          -- ^ Index of the element to remove from the 'FocusList'.
  -> FocusList a  -- ^ The 'FocusList' to remove an element from.
  -> (Int -> Int) -- ^ A function to use to update the
                  -- 'Focus' of the 'FocusList'.  This function is only used if
                  -- the index to remove from the 'FocusList' is the current
                  -- 'Focus'.
  -> Maybe (FocusList a)
removeFL i fl updateFocus =
  if i < 0 || i >= (fl ^. lensFocusListLen) || isEmptyFL fl
    then
      -- Return Nothing if the removal position is out of bounds.
      Nothing
    else
      undefined
