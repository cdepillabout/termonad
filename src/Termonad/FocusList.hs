{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Termonad.FocusList where

import Termonad.Prelude

import Control.Lens
import qualified Data.Foldable as Foldable
import Test.QuickCheck
import Text.Show (Show(showsPrec), ShowS, showParen, showString)

-- $setup
-- >>> :set -XFlexibleContexts
-- >>> :set -XScopedTypeVariables

data Focus = Focus {-# UNPACK #-} !Int | NoFocus deriving (Eq, Generic, Read, Show)

-- | 'NoFocus' is always less than 'Focus'.
--
-- prop> NoFocus < Focus a
instance Ord Focus where
  compare :: Focus -> Focus -> Ordering
  compare NoFocus NoFocus = EQ
  compare NoFocus (Focus _) = LT
  compare (Focus _) NoFocus = GT
  compare (Focus a) (Focus b) = compare a b

instance CoArbitrary Focus

foldFocus :: b -> (Int -> b) -> Focus -> b
foldFocus b _ NoFocus = b
foldFocus _ f (Focus i) = f i

_Focus :: Prism' Focus Int
_Focus = prism' Focus (foldFocus Nothing Just)

_NoFocus :: Prism' Focus ()
_NoFocus = prism' (const NoFocus) (foldFocus (Just ()) (const Nothing))

hasFocus :: Focus -> Bool
hasFocus NoFocus = False
hasFocus (Focus _) = True

unsafeGetFocus :: Focus -> Int
unsafeGetFocus NoFocus = error "unsafeGetFocus: NoFocus"
unsafeGetFocus (Focus i) = i

-- TODO: Probably be better
-- implemented as an Order statistic tree
-- (https://en.wikipedia.org/wiki/Order_statistic_tree).
data FocusList a = FocusList
  { focusListFocus :: !Focus
  , focusListLen :: {-# UNPACK #-} !Int
  , focusList :: !(IntMap a)
  } deriving (Eq, Generic)

$(makeLensesFor
    [ ("focusListFocus", "lensFocusListFocus")
    , ("focusListLen", "lensFocusListLen")
    , ("focusList", "lensFocusList")
    ]
    ''FocusList
 )

instance Functor FocusList where
  fmap :: (a -> b) -> FocusList a -> FocusList b
  fmap f (FocusList focus len intmap) = FocusList focus len (fmap f intmap)

instance Foldable FocusList where
  foldr f b (FocusList _ _ intmap) = Foldable.foldr f b intmap

instance Traversable FocusList where
  traverse :: Applicative f => (a -> f b) -> FocusList a -> f (FocusList b)
  traverse f (FocusList focus len intmap) = FocusList focus len <$> traverse f intmap

type instance Element (FocusList a) = a

instance MonoFunctor (FocusList a)

instance MonoFoldable (FocusList a)

instance MonoTraversable (FocusList a)

instance Arbitrary1 FocusList where
  liftArbitrary :: Gen a -> Gen (FocusList a)
  liftArbitrary genA = do
    arbList <- liftArbitrary genA
    case arbList of
      [] -> pure emptyFL
      (_:_) -> do
        let listLen = length arbList
        len <- choose (0, listLen - 1)
        pure $ unsafeFLFromList (Focus len) arbList

instance Arbitrary a => Arbitrary (FocusList a) where
  arbitrary = arbitrary1

instance CoArbitrary a => CoArbitrary (FocusList a)

debugFL :: Show a => FocusList a -> String
debugFL FocusList{..} =
  showString "FocusList {" .
  showString "focusListFocus = " .
  showsPrec 0 focusListFocus .
  showString ", " .
  showString "focusListLen = " .
  showsPrec 0 focusListLen .
  showString ", " .
  showString "focusList = " .
  showsPrec 0 focusList $
  showString "}" ""

instance Show a => Show (FocusList a) where
  showsPrec :: Int -> FocusList a -> ShowS
  showsPrec d FocusList{..} =
    let list = fmap snd $ sortOn fst $ mapToList focusList
    in
    showParen (d > 10) $
      showString "FocusList " .
      showsPrec 11 focusListFocus .
      showString " " .
      showsPrec 11 list

lensFocusListAt :: Int -> Lens' (FocusList a) (Maybe a)
lensFocusListAt i = lensFocusList . at i

-- | This is an invariant that the 'FocusList' must always protect.
invariantFL :: FocusList a -> Bool
invariantFL fl =
  invariantFocusNotNeg &&
  invariantFocusInMap &&
  invariantFocusIfLenGT0 &&
  invariantLenIsCorrect &&
  invariantNoSkippedNumsInMap
  where
    -- This makes sure that the 'Focus' in a 'FocusList' can never be negative.
    invariantFocusNotNeg :: Bool
    invariantFocusNotNeg =
      case fl ^. lensFocusListFocus of
        NoFocus -> True
        Focus i -> i >= 0

    -- | This makes sure that if there is a 'Focus', then it actually exists in
    -- the 'FocusList'.
    invariantFocusInMap :: Bool
    invariantFocusInMap =
      case fl ^. lensFocusListFocus of
        NoFocus -> length (fl ^. lensFocusList) == 0
        Focus i ->
          case lookup i (fl ^. lensFocusList) of
            Nothing -> False
            Just _ -> True

    -- | This makes sure that there needs to be a 'Focus' if the length of the
    -- 'FocusList' is greater than 0.
    invariantFocusIfLenGT0 :: Bool
    invariantFocusIfLenGT0 =
      let len = fl ^. lensFocusListLen
          focus = fl ^. lensFocusListFocus
      in
      case focus of
        Focus _ -> len /= 0
        NoFocus -> len == 0

    -- | Make sure that the length of the 'FocusList' is actually the number of
    -- elements in the inner 'IntMap'.
    invariantLenIsCorrect :: Bool
    invariantLenIsCorrect =
      let len = fl ^. lensFocusListLen
          intmap = fl ^. lensFocusList
      in len == length intmap

    -- | Make sure that there are no numbers that have been skipped in the
    -- inner 'IntMap'.
    invariantNoSkippedNumsInMap :: Bool
    invariantNoSkippedNumsInMap =
      let len = fl ^. lensFocusListLen
          intmap = fl ^. lensFocusList
          indexes = sort $ fmap fst $ mapToList intmap
      in indexes == [0..(len - 1)]


-- | Unsafely create a 'FocusList'.  This does not check that the focus
-- actually exists in the list.
--
-- >>> let fl = unsafeFLFromList (Focus 1) [0..2]
-- >>> debugFL fl
-- "FocusList {focusListFocus = Focus 1, focusListLen = 3, focusList = fromList [(0,0),(1,1),(2,2)]}"
--
-- >>> let fl = unsafeFLFromList NoFocus []
-- >>> debugFL fl
-- "FocusList {focusListFocus = NoFocus, focusListLen = 0, focusList = fromList []}"
unsafeFLFromList :: Focus -> [a] -> FocusList a
unsafeFLFromList focus list =
  let len = length list
  in
  FocusList
    { focusListFocus = focus
    , focusListLen = len
    , focusList = mapFromList $ zip [0..] list
    }

focusItemGetter :: Getter (FocusList a) (Maybe a)
focusItemGetter = to getFLFocusItem

-- | Safely create a 'FocusList' from a list.
--
-- >>> flFromList (Focus 1) ["cat","dog","goat"]
-- Just (FocusList (Focus 1) ["cat","dog","goat"])
--
-- >>> flFromList NoFocus []
-- Just (FocusList NoFocus [])
--
-- If the 'Focus' is out of range for the list, then 'Nothing' will be returned.
--
-- >>> flFromList (Focus (-1)) ["cat","dog","goat"]
-- Nothing
--
-- >>> flFromList (Focus 3) ["cat","dog","goat"]
-- Nothing
--
-- >>> flFromList NoFocus ["cat","dog","goat"]
-- Nothing
flFromList :: Focus -> [a] -> Maybe (FocusList a)
flFromList NoFocus [] = Just emptyFL
flFromList _ [] = Nothing
flFromList NoFocus (_:_) = Nothing
flFromList (Focus i) list =
  let len = length list
  in
  if i < 0 || i >= len
    then Nothing
    else
      Just $
        FocusList
          { focusListFocus = Focus i
          , focusListLen = len
          , focusList = mapFromList $ zip [0..] list
          }

-- | Create a 'FocusList' with a single element.
--
-- >>> singletonFL "hello"
-- FocusList (Focus 0) ["hello"]
singletonFL :: a -> FocusList a
singletonFL a =
  FocusList
    { focusListFocus = Focus 0
    , focusListLen = 1
    , focusList = singletonMap 0 a
    }

-- | Create an empty 'FocusList' without a 'Focus'.
--
-- >>> emptyFL
-- FocusList NoFocus []
emptyFL :: FocusList a
emptyFL =
  FocusList
    { focusListFocus = NoFocus
    , focusListLen = 0
    , focusList = mempty
    }

-- | Return 'True' if the 'FocusList' is empty.
--
-- >>> isEmptyFL emptyFL
-- True
--
-- >>> isEmptyFL $ singletonFL "hello"
-- False
--
-- Any 'FocusList' with a 'Focus' should never be empty.
isEmptyFL :: FocusList a -> Bool
isEmptyFL fl = fl ^. lensFocusListLen == 0

-- | Append a value to the end of a 'FocusList'.
--
-- This can be thought of as a \"snoc\" operation.
--
-- >>> appendFL emptyFL "hello"
-- FocusList (Focus 0) ["hello"]
--
-- >>> appendFL (singletonFL "hello") "bye"
-- FocusList (Focus 0) ["hello","bye"]
--
-- Appending a value to an empty 'FocusList' is the same as using 'singletonFL'.
--
-- prop> appendFL emptyFL a == singletonFL a
appendFL :: FocusList a -> a -> FocusList a
appendFL fl a =
  if isEmptyFL fl
    then singletonFL a
    else unsafeInsertNewFL (fl ^. lensFocusListLen) a fl

-- | A combination of 'appendFL' and 'setFocusFL'.
--
-- >>> let Just fl = flFromList (Focus 1) ["hello", "bye", "tree"]
-- >>> appendSetFocusFL fl "pie"
-- FocusList (Focus 3) ["hello","bye","tree","pie"]
--
-- prop> (appendSetFocusFL fl a) ^. lensFocusListFocus /= fl ^. lensFocusListFocus
appendSetFocusFL :: FocusList a -> a -> FocusList a
appendSetFocusFL fl a =
  let oldLen = fl ^. lensFocusListLen
  in
  case setFocusFL oldLen (appendFL fl a) of
    Nothing -> error "Internal error with setting the focus.  This should never happen."
    Just newFL -> newFL

-- | Prepend a value to a 'FocusList'.
--
-- This can be thought of as a \"cons\" operation.
--
-- >>> prependFL "hello" emptyFL
-- FocusList (Focus 0) ["hello"]
--
-- The focus will be updated when prepending:
--
-- >>> prependFL "bye" (singletonFL "hello")
-- FocusList (Focus 1) ["bye","hello"]
--
-- Prepending to a 'FocusList' will always update the 'Focus':
--
-- prop> (fl ^. lensFocusListFocus) < (prependFL a fl ^. lensFocusListFocus)
prependFL :: a -> FocusList a -> FocusList a
prependFL a fl =
  if isEmptyFL fl
    then singletonFL a
    else unsafeInsertNewFL 0 a $ unsafeShiftUpFrom 0 fl

-- | Unsafely get the 'Focus' from a 'FocusList'.  If the 'Focus' is
-- 'NoFocus', this function returns 'error'.
unsafeGetFLFocus :: FocusList a -> Int
unsafeGetFLFocus fl =
  let focus = fl ^. lensFocusListFocus
  in
  case focus of
    NoFocus -> error "unsafeGetFLFocus: the focus list doesn't have a focus"
    Focus i -> i

-- | Unsafely get the value of the 'Focus' from a 'FocusList'.  If the 'Focus' is
-- 'NoFocus', this function returns 'error'.
unsafeGetFLFocusItem :: FocusList a -> a
unsafeGetFLFocusItem fl =
  let focus = fl ^. lensFocusListFocus
  in
  case focus of
    NoFocus -> error "unsafeGetFLFocusItem: the focus list doesn't have a focus"
    Focus i ->
      let intmap = fl ^. lensFocusList
      in
      case lookup i intmap of
        Nothing ->
          error $
            "unsafeGetFLFocusItem: internal error, i (" <>
            show i <>
            ") doesnt exist in intmap"
        Just a -> a

getFLFocusItem :: FocusList a -> Maybe a
getFLFocusItem fl =
  let focus = fl ^. lensFocusListFocus
  in
  case focus of
    NoFocus -> Nothing
    Focus i ->
      let intmap = fl ^. lensFocusList
      in
      case lookup i intmap of
        Nothing ->
          error $
            "getFLFocusItem: internal error, i (" <>
            show i <>
            ") doesnt exist in intmap"
        Just a -> Just a

-- | Unsafely insert a new @a@ in a 'FocusList'.  This sets the 'Int' value to
-- @a@.  The length of the 'FocusList' will be increased by 1.  The
-- 'FocusList's 'Focus' is not changed.
--
-- If there is some value in the 'FocusList' already at the 'Int', then it will
-- be overwritten.  Also, the 'Int' is not checked to make sure it is above 0.
--
-- This function is meant to be used after 'unsafeShiftUpFrom'.
--
-- >>> let fl = unsafeShiftUpFrom 2 $ unsafeFLFromList (Focus 1) [0,1,200]
-- >>> debugFL $ unsafeInsertNewFL 2 100 fl
-- "FocusList {focusListFocus = Focus 1, focusListLen = 4, focusList = fromList [(0,0),(1,1),(2,100),(3,200)]}"
--
-- >>> let fl = unsafeFLFromList NoFocus []
-- >>> debugFL $ unsafeInsertNewFL 0 100 fl
-- "FocusList {focusListFocus = NoFocus, focusListLen = 1, focusList = fromList [(0,100)]}"
unsafeInsertNewFL :: Int -> a -> FocusList a -> FocusList a
unsafeInsertNewFL i a fl =
  fl &
    lensFocusListLen +~ 1 &
    lensFocusListAt i ?~ a

-- | This unsafely shifts all values up in a 'FocusList' starting at a given
-- index.  It also updates the 'Focus' of the 'FocusList' if it has been
-- shifted.  This does not change the length of the 'FocusList'.
--
-- It does not check that the 'Int' is greater than 0.  It also does not check
-- that there is a 'Focus'.
--
-- ==== __EXAMPLES__
--
-- >>> let fl = unsafeShiftUpFrom 2 $ unsafeFLFromList (Focus 1) [0,1,200]
-- >>> debugFL fl
-- "FocusList {focusListFocus = Focus 1, focusListLen = 3, focusList = fromList [(0,0),(1,1),(3,200)]}"
--
-- >>> let fl = unsafeShiftUpFrom 1 $ unsafeFLFromList (Focus 1) [0,1,200]
-- >>> debugFL fl
-- "FocusList {focusListFocus = Focus 2, focusListLen = 3, focusList = fromList [(0,0),(2,1),(3,200)]}"
--
-- >>> let fl = unsafeShiftUpFrom 0 $ unsafeFLFromList (Focus 1) [0,1,200]
-- >>> debugFL fl
-- "FocusList {focusListFocus = Focus 2, focusListLen = 3, focusList = fromList [(1,0),(2,1),(3,200)]}"
--
-- >>> let fl = unsafeShiftUpFrom 0 $ unsafeFLFromList (Focus 1) [0,1,200]
-- >>> debugFL fl
-- "FocusList {focusListFocus = Focus 2, focusListLen = 3, focusList = fromList [(1,0),(2,1),(3,200)]}"
unsafeShiftUpFrom :: forall a. Int -> FocusList a -> FocusList a
unsafeShiftUpFrom i fl =
  let intMap = fl ^. lensFocusList
      lastElemIdx = (fl ^. lensFocusListLen) - 1
      newIntMap = go i lastElemIdx intMap
      oldFocus = unsafeGetFLFocus fl
      newFocus = if i > oldFocus then oldFocus else oldFocus + 1
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

lookupFL :: Int -> FocusList a -> Maybe a
lookupFL i fl = lookup i (fl ^. lensFocusList)

-- | Insert a new value into the 'FocusList'.  The 'Focus' of the list is
-- changed appropriately.
--
-- >>> insertFL 0 "hello" emptyFL
-- Just (FocusList (Focus 0) ["hello"])
--
-- >>> insertFL 0 "hello" (singletonFL "bye")
-- Just (FocusList (Focus 1) ["hello","bye"])
--
-- >>> insertFL 1 "hello" (singletonFL "bye")
-- Just (FocusList (Focus 0) ["bye","hello"])
--
-- This returns 'Nothing' if the index at which to insert the new value is
-- either less than 0 or greater than the length of the list.
--
-- >>> insertFL 100 "hello" emptyFL
-- Nothing
--
-- >>> insertFL 100 "bye" (singletonFL "hello")
-- Nothing
--
-- >>> insertFL (-1) "bye" (singletonFL "hello")
-- Nothing
insertFL
  :: Int  -- ^ The index at which to insert the value.
  -> a
  -> FocusList a
  -> Maybe (FocusList a)
insertFL i a fl
  | i < 0 || i > (fl ^. lensFocusListLen) =
    -- Return Nothing if the insertion position is out of bounds.
    Nothing
  | i == 0 && isEmptyFL fl =
    -- Return a 'FocusList' with one element if the insertion position is 0
    -- and the 'FocusList' is empty.
    Just $ singletonFL a
  | otherwise =
     -- Shift all existing values up one and insert the new
     -- value in the opened place.
     let shiftedUpFL = unsafeShiftUpFrom i fl
     in Just $ unsafeInsertNewFL i a shiftedUpFL

-- | Unsafely remove a value from a 'FocusList'.  It effectively leaves a hole
-- inside the 'FocusList'.  It updates the length of the 'FocusList'.
--
-- This function does not check that a value actually exists in the
-- 'FocusList'.  It also does not update the 'Focus'.
--
-- This function does update the length of the 'FocusList'.
--
-- >>> debugFL $ unsafeRemove 1 $ unsafeFLFromList (Focus 0) [0..2]
-- "FocusList {focusListFocus = Focus 0, focusListLen = 2, focusList = fromList [(0,0),(2,2)]}"
--
-- >>> debugFL $ unsafeRemove 0 $ unsafeFLFromList (Focus 0) [0..2]
-- "FocusList {focusListFocus = Focus 0, focusListLen = 2, focusList = fromList [(1,1),(2,2)]}"
--
-- Trying to remove the last element is completely safe (unless, of course, it
-- is the 'Focus'):
--
-- >>> debugFL $ unsafeRemove 2 $ unsafeFLFromList (Focus 2) [0..2]
-- "FocusList {focusListFocus = Focus 2, focusListLen = 2, focusList = fromList [(0,0),(1,1)]}"
--
-- If this function is passed an empty 'FocusList', it will make the length -1.
--
-- >>> debugFL $ unsafeRemove 0 emptyFL
-- "FocusList {focusListFocus = NoFocus, focusListLen = -1, focusList = fromList []}"
unsafeRemove
  :: Int
  -> FocusList a
  -> FocusList a
unsafeRemove i fl =
  fl &
    lensFocusListLen -~ 1 &
    lensFocusListAt i .~ Nothing

-- | This shifts all the values down in a 'FocusList' starting at a given
-- index.  It does not change the 'Focus' of the 'FocusList'.  It does not change the
-- length of the 'FocusList'.
--
-- It does not check that shifting elements down will not overwrite other elements.
-- This function is meant to be called after 'unsafeRemove'.
--
-- >>> let fl = unsafeRemove 1 $ unsafeFLFromList (Focus 0) [0..2]
-- >>> debugFL $ unsafeShiftDownFrom 1 fl
-- "FocusList {focusListFocus = Focus 0, focusListLen = 2, focusList = fromList [(0,0),(1,2)]}"
--
-- >>> let fl = unsafeRemove 0 $ unsafeFLFromList (Focus 0) [0..2]
-- >>> debugFL $ unsafeShiftDownFrom 0 fl
-- "FocusList {focusListFocus = Focus 0, focusListLen = 2, focusList = fromList [(0,1),(1,2)]}"
--
-- Trying to shift down from the last element after it has been removed is a no-op:
--
-- >>> let fl = unsafeRemove 2 $ unsafeFLFromList (Focus 0) [0..2]
-- >>> debugFL $ unsafeShiftDownFrom 2 fl
-- "FocusList {focusListFocus = Focus 0, focusListLen = 2, focusList = fromList [(0,0),(1,1)]}"
unsafeShiftDownFrom :: forall a. Int -> FocusList a -> FocusList a
unsafeShiftDownFrom i fl =
  let intMap = fl ^. lensFocusList
      len = fl ^. lensFocusListLen
      newIntMap = go (i + 1) len intMap
  in fl & lensFocusList .~ newIntMap
  where
    go :: Int -> Int -> IntMap a -> IntMap a
    go idxToShiftDown len intMap
      | idxToShiftDown < len + 1 =
        let val = unsafeLookup idxToShiftDown intMap
            newMap =
              insertMap (idxToShiftDown - 1) val (deleteMap idxToShiftDown intMap)
        in go (idxToShiftDown + 1) len newMap
      | otherwise = intMap

-- | Remove an element from a 'FocusList'.
--
-- If the element to remove is not the 'Focus', then update the 'Focus'
-- accordingly.
--
-- For example, if the 'Focus' is on index 1, and we have removed index 2, then
-- the focus is not affected, so it is not changed.
--
-- >>> let focusList = unsafeFLFromList (Focus 1) ["cat","goat","dog","hello"]
-- >>> removeFL 2 focusList
-- Just (FocusList (Focus 1) ["cat","goat","hello"])
--
-- If the 'Focus' is on index 2 and we have removed index 1, then the 'Focus'
-- will be moved back one element to set to index 1.
--
-- >>> let focusList = unsafeFLFromList (Focus 2) ["cat","goat","dog","hello"]
-- >>> removeFL 1 focusList
-- Just (FocusList (Focus 1) ["cat","dog","hello"])
--
-- If we remove the 'Focus', then the next item is set to have the 'Focus'.
--
-- >>> let focusList = unsafeFLFromList (Focus 0) ["cat","goat","dog","hello"]
-- >>> removeFL 0 focusList
-- Just (FocusList (Focus 0) ["goat","dog","hello"])
--
-- If the element to remove is the only element in the list, then the 'Focus'
-- will be set to 'NoFocus'.
--
-- >>> let focusList = unsafeFLFromList (Focus 0) ["hello"]
-- >>> removeFL 0 focusList
-- Just (FocusList NoFocus [])
--
-- If the 'Int' for the index to remove is either less than 0 or greater then
-- the length of the list, then 'Nothing' is returned.
--
-- >>> let focusList = unsafeFLFromList (Focus 0) ["hello"]
-- >>> removeFL (-1) focusList
-- Nothing
--
-- >>> let focusList = unsafeFLFromList (Focus 1) ["hello","bye","cat"]
-- >>> removeFL 3 focusList
-- Nothing
--
-- If the 'FocusList' passed in is 'Empty', then 'Nothing' is returned.
--
-- >>> removeFL 0 emptyFL
-- Nothing
removeFL
  :: Int          -- ^ Index of the element to remove from the 'FocusList'.
  -> FocusList a  -- ^ The 'FocusList' to remove an element from.
  -> Maybe (FocusList a)
removeFL i fl
  | i < 0 || i >= (fl ^. lensFocusListLen) || isEmptyFL fl =
    -- Return Nothing if the removal position is out of bounds.
    Nothing
  | fl ^. lensFocusListLen == 1 =
    -- Return an empty focus list if there is currently only one element
    Just emptyFL
  | otherwise =
    let newFLWithHole = unsafeRemove i fl
        newFL = unsafeShiftDownFrom i newFLWithHole
        focus = unsafeGetFLFocus fl
    in
    if focus >= i && focus /= 0
      then Just $ newFL & lensFocusListFocus . _Focus -~ 1
      else Just newFL

-- | Find the index of the first element in the 'FocusList'.
--
-- >>> let Just fl = flFromList (Focus 1) ["hello", "bye", "tree"]
-- >>> indexOfFL "hello" fl
-- Just 0
--
-- If more than one element exists, then return the index of the first one.
--
-- >>> let Just fl = flFromList (Focus 1) ["dog", "cat", "cat"]
-- >>> indexOfFL "cat" fl
-- Just 1
--
-- If the element doesn't exist, then return 'Nothing'
--
-- >>> let Just fl = flFromList (Focus 1) ["foo", "bar", "baz"]
-- >>> indexOfFL "hogehoge" fl
-- Nothing
indexOfFL :: Eq a => a -> FocusList a -> Maybe Int
indexOfFL a fl =
  let intmap = focusList fl
      keyVals = sortOn fst $ mapToList intmap
      maybeKeyVal = find (\(_, val) -> val == a) keyVals
  in fmap fst maybeKeyVal

-- | Delete an element from a 'FocusList'.
--
-- >>> let Just fl = flFromList (Focus 0) ["hello", "bye", "tree"]
-- >>> deleteFL "bye" fl
-- FocusList (Focus 0) ["hello","tree"]
--
-- The focus will be updated if an item before it is deleted.
--
-- >>> let Just fl = flFromList (Focus 1) ["hello", "bye", "tree"]
-- >>> deleteFL "hello" fl
-- FocusList (Focus 0) ["bye","tree"]
--
-- If there are multiple matching elements in the 'FocusList', remove them all.
--
-- >>> let Just fl = flFromList (Focus 0) ["hello", "bye", "bye"]
-- >>> deleteFL "bye" fl
-- FocusList (Focus 0) ["hello"]
--
-- If there are no matching elements, return the original 'FocusList'.
--
-- >>> let Just fl = flFromList (Focus 2) ["hello", "good", "bye"]
-- >>> deleteFL "frog" fl
-- FocusList (Focus 2) ["hello","good","bye"]
deleteFL
  :: forall a.
     (Eq a)
  => a
  -> FocusList a
  -> FocusList a
deleteFL item = go
  where
    go :: FocusList a -> FocusList a
    go fl =
      let maybeIndex = indexOfFL item fl
      in
      case maybeIndex of
        Nothing -> fl
        Just i ->
          let maybeNewFL = removeFL i fl
          in
          case maybeNewFL of
            Nothing -> fl
            Just newFL -> go newFL

-- | Set the 'Focus' for a 'FocusList'.
--
-- This is just like 'updateFocusFL', but doesn't return the new focused item.
--
-- prop> setFocusFL i fl == fmap snd (updateFocusFL i fl)
setFocusFL :: Int -> FocusList a -> Maybe (FocusList a)
setFocusFL i fl
  -- Can't set a 'Focus' for an empty 'FocusList'.
  | isEmptyFL fl = Nothing
  | otherwise =
    let len = fl ^. lensFocusListLen
    in
    if i < 0 || i >= len
      then Nothing
      else Just $ fl & lensFocusListFocus . _Focus .~ i

-- | Update the 'Focus' for a 'FocusList' and get the new focused element.
--
-- >>> updateFocusFL 1 =<< flFromList (Focus 2) ["hello","bye","dog","cat"]
-- Just ("bye",FocusList (Focus 1) ["hello","bye","dog","cat"])
--
-- If the 'FocusList' is empty, then return 'Nothing'.
--
-- >>> updateFocusFL 1 emptyFL
-- Nothing
--
-- If the new focus is less than 0, or greater than or equal to the length of
-- the 'FocusList', then return 'Nothing'.
--
-- >>> updateFocusFL (-1) =<< flFromList (Focus 2) ["hello","bye","dog","cat"]
-- Nothing
--
-- >>> updateFocusFL 4 =<< flFromList (Focus 2) ["hello","bye","dog","cat"]
-- Nothing
updateFocusFL :: Int -> FocusList a -> Maybe (a, FocusList a)
updateFocusFL i fl
  | isEmptyFL fl = Nothing
  | otherwise =
    let len = fl ^. lensFocusListLen
    in
    if i < 0 || i >= len
      then Nothing
      else
        let newFL = fl & lensFocusListFocus . _Focus .~ i
        in Just (unsafeGetFLFocusItem newFL, newFL)
