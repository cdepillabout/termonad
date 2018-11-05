{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Termonad.FocusList
  (
    -- * FocusList
    FocusList(FocusList, focusListFocus, focusList)
    -- ** Conversions
  , fromListFL
  , fromFoldable
  , toSeqFL
    -- ** Query
  , lengthFL
  , isEmptyFL
  , getFocusItemFL
  , lookupFL
  , indexOfFL
  , findFL
    -- *** Query 'Focus'
  , hasFocusFL
  , getFocusFL
    -- ** Manipulate
  , prependFL
  , appendFL
  , appendSetFocusFL
  , insertFL
  , removeFL
  , deleteFL
  , moveFromToFL
  , intersperseFL
  , reverseFL
    -- *** Manipulate 'Focus'
  , setFocusFL
  , updateFocusFL
    -- ** Sort
  , sortByFL
    -- ** Construction
  , emptyFL
  , singletonFL
    -- ** Unsafe Functions
  , unsafeFromListFL
  , unsafeGetFocusFL
  , unsafeGetFocusItemFL
    -- ** Invariants
  , invariantFL
    -- ** Testing
  , genValidFL
    -- ** Optics
    -- | These optics allow you to get/set the internal state of a 'FocusList'.
    -- You should make sure not to directly set the internal state of a
    -- 'FocusList' unless you are sure that the invariants for the 'FocusList'
    -- are protected.  See 'invariantFL'.
  , lensFocusListFocus
  , lensFocusList
    -- * Focus
  , Focus(Focus, NoFocus)
    , hasFocus
    , getFocus
    , maybeToFocus
    , foldFocus
    -- ** Optics
  , _Focus
  , _NoFocus
    -- ** Unsafe Functions
  , unsafeGetFocus
  ) where

import Termonad.Prelude

import Control.Lens (Prism', (^.), (.~), (-~), makeLensesFor, prism')
import qualified Data.Foldable as Foldable
import qualified Data.Sequence as S
import Data.Sequence (Seq((:<|), Empty))
import Test.QuickCheck
import Text.Show (Show(showsPrec), ShowS, showParen, showString)

-- $setup
-- >>> :set -XFlexibleContexts
-- >>> :set -XScopedTypeVariables

-- | A 'Focus' for the 'FocusList'.
--
-- The 'Focus' is either 'NoFocus' (if the 'Focuslist' is empty), or 'Focus'
-- 'Int' to represent focusing on a specific element of the 'FocusList'.
data Focus = Focus {-# UNPACK #-} !Int | NoFocus deriving (Eq, Generic, Read, Show)

-- | 'NoFocus' is always less than 'Focus'.
--
-- prop> NoFocus < Focus a
--
-- The ordering of 'Focus' depends on the ordering of the integer contained
-- inside.
--
-- prop> (a < b) ==> (Focus a < Focus b)
instance Ord Focus where
  compare :: Focus -> Focus -> Ordering
  compare NoFocus NoFocus = EQ
  compare NoFocus (Focus _) = LT
  compare (Focus _) NoFocus = GT
  compare (Focus a) (Focus b) = compare a b

instance CoArbitrary Focus

instance Arbitrary Focus where
  arbitrary = frequency [(1, pure NoFocus), (3, fmap Focus arbitrary)]

-- | A fold function for 'Focus'.
--
-- This is similar to 'maybe' for 'Maybe'.
--
-- >>> foldFocus "empty" (\i -> "focus at " <> show i) (Focus 3)
-- "focus at 3"
--
-- >>> foldFocus Nothing Just NoFocus
-- Nothing
--
-- prop> foldFocus NoFocus Focus focus == focus
foldFocus :: b -> (Int -> b) -> Focus -> b
foldFocus b _ NoFocus = b
foldFocus _ f (Focus i) = f i

-- | A 'Prism'' for focusing on the 'Focus' constructor in a 'Focus' data type.
_Focus :: Prism' Focus Int
_Focus = prism' Focus (foldFocus Nothing Just)

-- | A 'Prism'' for focusing on the 'NoFocus' constructor in a 'Focus' data type.
_NoFocus :: Prism' Focus ()
_NoFocus = prism' (const NoFocus) (foldFocus (Just ()) (const Nothing))

-- | Returns 'True' if a 'Focus' exists, and 'False' if not.
--
-- >>> hasFocus (Focus 0)
-- True
--
-- >>> hasFocus NoFocus
-- False
--
-- @O(1)@
hasFocus :: Focus -> Bool
hasFocus NoFocus = False
hasFocus (Focus _) = True

-- | Get the focus index from a 'Focus'.
--
-- >>> getFocus (Focus 3)
-- Just 3
--
-- >>> getFocus NoFocus
-- Nothing
--
-- @O(1)@
getFocus :: Focus -> Maybe Int
getFocus NoFocus = Nothing
getFocus (Focus i) = Just i

-- | Convert a 'Maybe' 'Int' to a 'Focus'.
--
-- >>> maybeToFocus (Just 100)
-- Focus 100
--
-- >>> maybeToFocus Nothing
-- NoFocus
--
-- 'maybeToFocus' and 'getFocus' witness an isomorphism.
--
-- prop> focus == maybeToFocus (getFocus focus)
--
-- prop> maybeInt == getFocus (maybeToFocus maybeInt)
--
-- @O(1)@
maybeToFocus :: Maybe Int -> Focus
maybeToFocus Nothing = NoFocus
maybeToFocus (Just i) = Focus i

-- | Unsafely get the focus index from a 'Focus'.
--
-- Returns an error if 'NoFocus'.
--
-- >>> unsafeGetFocus (Focus 50)
-- 50
--
-- >>> unsafeGetFocus NoFocus
-- *** Exception: ...
-- ...
--
-- @O(1)@
unsafeGetFocus :: Focus -> Int
unsafeGetFocus NoFocus = error "unsafeGetFocus: NoFocus"
unsafeGetFocus (Focus i) = i

-- | A list with a given element having the 'Focus'.
--
-- Implemented under the hood as a 'S.Seq'.  The 'FocusList' has some
-- invariants that must be protected.  You should not use the 'FocusList'
-- constructor or the 'focusListFocus' or 'focusList' accessors.
data FocusList a = FocusList
  { focusListFocus :: !Focus
  , focusList :: !(S.Seq a)
  } deriving (Eq, Functor, Generic)

$(makeLensesFor
    [ ("focusListFocus", "lensFocusListFocus")
    , ("focusList", "lensFocusList")
    ]
    ''FocusList
 )

instance Foldable FocusList where
  foldr f b (FocusList _ fls) = Foldable.foldr f b fls

  length = lengthFL

instance Traversable FocusList where
  traverse :: Applicative f => (a -> f b) -> FocusList a -> f (FocusList b)
  traverse f (FocusList focus fls) = FocusList focus <$> traverse f fls

type instance Element (FocusList a) = a

instance MonoFunctor (FocusList a)

instance MonoFoldable (FocusList a) where
  olength = lengthFL

instance MonoTraversable (FocusList a)

instance GrowingAppend (FocusList a)

instance SemiSequence (FocusList a) where
  type Index (FocusList a) = Int

  intersperse = intersperseFL

  reverse = reverseFL

  find = findFL

  sortBy = sortByFL

  cons = prependFL

  snoc = appendFL

-- | Given a 'Gen' for @a@, generate a valid 'FocusList'.
genValidFL :: forall a. Gen a -> Gen (FocusList a)
genValidFL genA = do
  newFL <- genFL
  if invariantFL newFL
    then pure newFL
    else error "genValidFL generated an invalid FocusList!  This should never happen!"
  where
    genFL :: Gen (FocusList a)
    genFL = do
      arbList <- liftArbitrary genA
      case arbList of
        [] -> pure emptyFL
        (_:_) -> do
          let listLen = length arbList
          len <- choose (0, listLen - 1)
          pure $ unsafeFromListFL (Focus len) arbList

instance Arbitrary1 FocusList where
  liftArbitrary = genValidFL

instance Arbitrary a => Arbitrary (FocusList a) where
  arbitrary = arbitrary1

instance CoArbitrary a => CoArbitrary (FocusList a)

instance Show a => Show (FocusList a) where
  showsPrec :: Int -> FocusList a -> ShowS
  showsPrec d FocusList{..} =
    showParen (d > 10) $
      showString "FocusList " .
      showsPrec 11 focusListFocus .
      showString " " .
      showsPrec 11 (toList focusList)

-- | Get the underlying 'Seq' in a 'FocusList'.
--
-- @O(1)@
toSeqFL :: FocusList a -> Seq a
toSeqFL FocusList{focusList = fls} = fls

-- | Return the length of a 'FocusList'.
--
-- >>> let Just fl = fromListFL (Focus 2) ["hello", "bye", "parrot"]
-- >>> lengthFL fl
-- 3
--
-- @O(1)@
lengthFL :: FocusList a -> Int
lengthFL = S.length . focusList

-- | This is an invariant that the 'FocusList' must always protect.
--
-- The functions in this module should generally protect this invariant.  If
-- they do not, it is generally a bug.
--
-- The invariants are as follows:
--
-- - The 'Focus' in a 'FocusList' can never be negative.
--
-- - If there is a 'Focus', then it actually exists in
--   the 'FocusList'.
--
-- - There needs to be a 'Focus' if the length of the
--   'FocusList' is greater than 0.
--
-- @O(log n)@, where @n@ is the length of the 'FocusList'.
invariantFL :: FocusList a -> Bool
invariantFL fl =
  invariantFocusNotNeg &&
  invariantFocusInMap &&
  invariantFocusIfLenGT0
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
          case S.lookup i (fl ^. lensFocusList) of
            Nothing -> False
            Just _ -> True

    -- | This makes sure that there needs to be a 'Focus' if the length of the
    -- 'FocusList' is greater than 0.
    invariantFocusIfLenGT0 :: Bool
    invariantFocusIfLenGT0 =
      let len = lengthFL fl
          focus = fl ^. lensFocusListFocus
      in
      case focus of
        Focus _ -> len /= 0
        NoFocus -> len == 0

-- | Unsafely create a 'FocusList'.  This does not check that the focus
-- actually exists in the list.  This is an internal function and should
-- generally not be used.  It is only safe to use if you ALREADY know
-- the 'Focus' is within the list.
--
-- Instead, you should generally use 'fromListFL'.
--
-- The following is an example of using 'unsafeFromListFL' correctly.
--
-- >>> unsafeFromListFL (Focus 1) [0..2]
-- FocusList (Focus 1) [0,1,2]
--
-- >>> unsafeFromListFL NoFocus []
-- FocusList NoFocus []
--
-- 'unsafeFromListFL' can also be used uncorrectly.  The following is an
-- example of 'unsafeFromListFL' allowing you to create a 'FocusList' that does
-- not pass 'invariantFL'.
--
-- >>> unsafeFromListFL (Focus 100) [0..2]
-- FocusList (Focus 100) [0,1,2]
--
-- If 'fromListFL' returns a 'Just' 'FocusList', then 'unsafeFromListFL' should
-- return the same 'FocusList'.
--
-- @O(n)@ where @n@ is the length of the input list.
unsafeFromListFL :: Focus -> [a] -> FocusList a
unsafeFromListFL focus list =
  FocusList
    { focusListFocus = focus
    , focusList = S.fromList list
    }

-- | Safely create a 'FocusList' from a list.
--
-- >>> fromListFL (Focus 1) ["cat","dog","goat"]
-- Just (FocusList (Focus 1) ["cat","dog","goat"])
--
-- >>> fromListFL NoFocus []
-- Just (FocusList NoFocus [])
--
-- If the 'Focus' is out of range for the list, then 'Nothing' will be returned.
--
-- >>> fromListFL (Focus (-1)) ["cat","dog","goat"]
-- Nothing
--
-- >>> fromListFL (Focus 3) ["cat","dog","goat"]
-- Nothing
--
-- >>> fromListFL NoFocus ["cat","dog","goat"]
-- Nothing
--
-- @O(n)@ where @n@ is the length of the input list.
fromListFL :: Focus -> [a] -> Maybe (FocusList a)
fromListFL NoFocus [] = Just emptyFL
fromListFL _ [] = Nothing
fromListFL NoFocus (_:_) = Nothing
fromListFL (Focus i) list =
  let len = length list
  in
  if i < 0 || i >= len
    then Nothing
    else
      Just $
        FocusList
          { focusListFocus = Focus i
          , focusList = S.fromList list
          }

-- | Create a 'FocusList' from any 'Foldable' container.
--
-- This just calls 'toList' on the 'Foldable', and then passes the result to
-- 'fromListFL'.
--
-- prop> fromFoldableFL foc (foldable :: Data.Sequence.Seq Int) == fromListFL foc (toList foldable)
--
-- @O(n)@
fromFoldableFL :: Foldable f => Focus -> f a -> Maybe (FocusList a)
fromFoldableFL foc as = fromListFL foc (Foldable.toList as)

-- | Create a 'FocusList' with a single element.
--
-- >>> singletonFL "hello"
-- FocusList (Focus 0) ["hello"]
--
-- @O(1)@
singletonFL :: a -> FocusList a
singletonFL a =
  FocusList
    { focusListFocus = Focus 0
    , focusList = S.singleton a
    }

-- | Create an empty 'FocusList' without a 'Focus'.
--
-- >>> emptyFL
-- FocusList NoFocus []
--
-- @O(1)@
emptyFL :: FocusList a
emptyFL =
  FocusList
    { focusListFocus = NoFocus
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
--
-- prop> hasFocusFL fl ==> not (isEmptyFL fl)
--
-- The opposite is also true.
--
-- @O(1)@
isEmptyFL :: FocusList a -> Bool
isEmptyFL fl = (lengthFL fl) == 0

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
--
-- @O(log n)@ where @n@ is the length of the 'FocusList'.
appendFL :: FocusList a -> a -> FocusList a
appendFL fl a =
  if isEmptyFL fl
    then singletonFL a
    else insertFL (length $ focusList fl) a fl

-- | A combination of 'appendFL' and 'setFocusFL'.
--
-- >>> let Just fl = fromListFL (Focus 1) ["hello", "bye", "tree"]
-- >>> appendSetFocusFL fl "pie"
-- FocusList (Focus 3) ["hello","bye","tree","pie"]
--
-- The 'Focus' will always be updated after calling 'appendSetFocusFL'.
--
-- prop> (appendSetFocusFL fl a) ^. lensFocusListFocus /= fl ^. lensFocusListFocus
--
-- @O(log n)@ where @n@ is the length of the 'FocusList'.
appendSetFocusFL :: FocusList a -> a -> FocusList a
appendSetFocusFL fl a =
  let oldLen = length $ focusList fl
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
--
-- @O(1)@
prependFL :: a -> FocusList a -> FocusList a
prependFL a fl@FocusList{ focusListFocus = focus, focusList = fls}  =
  case focus of
    NoFocus -> singletonFL a
    Focus i ->
      fl
        { focusListFocus = Focus (i+1)
        , focusList = a S.<| fls
        }

-- | Unsafely get the 'Focus' from a 'FocusList'.  If the 'Focus' is
-- 'NoFocus', this function returns 'error'.
--
-- This function is only safe if you already have knowledge that
-- the 'FocusList' has a 'Focus'.
--
-- Generally, 'getFocusFL' should be used instead of this function.
--
-- >>> let Just fl = fromListFL (Focus 1) [0..9]
-- >>> unsafeGetFocusFL fl
-- 1
--
-- >>> unsafeGetFocusFL emptyFL
-- *** Exception: ...
-- ...
--
-- @O(1)@
unsafeGetFocusFL :: FocusList a -> Int
unsafeGetFocusFL fl =
  let focus = fl ^. lensFocusListFocus
  in
  case focus of
    NoFocus -> error "unsafeGetFocusFL: the focus list doesn't have a focus"
    Focus i -> i

-- | Return 'True' if the 'Focus' in a 'FocusList' exists.
--
-- Return 'False' if the 'Focus' in a 'FocusList' is 'NoFocus'.
--
-- >>> hasFocusFL $ singletonFL "hello"
-- True
--
-- >>> hasFocusFL emptyFL
-- False
--
-- @O(1)@
hasFocusFL :: FocusList a -> Bool
hasFocusFL = hasFocus . getFocusFL

-- | Get the 'Focus' from a 'FocusList'.
--
-- >>> getFocusFL $ singletonFL "hello"
-- Focus 0
--
-- >>> let Just fl = fromListFL (Focus 3) [0..9]
-- >>> getFocusFL fl
-- Focus 3
--
-- >>> getFocusFL emptyFL
-- NoFocus
--
-- @O(1)@
getFocusFL :: FocusList a -> Focus
getFocusFL FocusList{focusListFocus} = focusListFocus

-- | Unsafely get the value of the 'Focus' from a 'FocusList'.  If the 'Focus' is
-- 'NoFocus', this function returns 'error'.
--
-- This function is only safe if you already have knowledge that the 'FocusList'
-- has a 'Focus'.
--
-- Generally, 'getFocusItemFL' should be used instead of this function.
--
-- >>> let Just fl = fromListFL (Focus 0) ['a'..'c']
-- >>> unsafeGetFocusItemFL fl
-- 'a'
--
-- >>> unsafeGetFocusFL emptyFL
-- *** Exception: ...
-- ...
--
-- @O(log(min(i, n - i)))@ where @i@ is the 'Focus', and @n@
-- is the length of the 'FocusList'.
unsafeGetFocusItemFL :: FocusList a -> a
unsafeGetFocusItemFL fl =
  let focus = fl ^. lensFocusListFocus
  in
  case focus of
    NoFocus -> error "unsafeGetFocusItemFL: the focus list doesn't have a focus"
    Focus i ->
      let fls = fl ^. lensFocusList
      in
      case S.lookup i fls of
        Nothing ->
          error $
            "unsafeGetFocusItemFL: internal error, i (" <>
            show i <>
            ") doesnt exist in sequence"
        Just a -> a

-- | Get the item the 'FocusList' is focusing on.  Return 'Nothing' if the
-- 'FocusList' is empty.
--
-- >>> let Just fl = fromListFL (Focus 0) ['a'..'c']
-- >>> getFocusItemFL fl
-- Just 'a'
--
-- >>> getFocusItemFL emptyFL
-- Nothing
--
-- This will always return 'Just' if there is a 'Focus'.
--
-- prop> hasFocusFL fl ==> isJust (getFocusItemFL fl)
--
-- @O(log(min(i, n - i)))@ where @i@ is the 'Focus', and @n@
-- is the length of the 'FocusList'.
getFocusItemFL :: FocusList a -> Maybe a
getFocusItemFL fl =
  let focus = fl ^. lensFocusListFocus
  in
  case focus of
    NoFocus -> Nothing
    Focus i ->
      let fls = fl ^. lensFocusList
      in
      case S.lookup i fls of
        Nothing ->
          error $
            "getFocusItemFL: internal error, i (" <>
            show i <>
            ") doesnt exist in sequence"
        Just a -> Just a

-- | Lookup the element at the specified index, counting from 0.
--
-- >>> let Just fl = fromListFL (Focus 0) ['a'..'c']
-- >>> lookupFL 0 fl
-- Just 'a'
--
-- Returns 'Nothing' if the index is out of bounds.
--
-- >>> let Just fl = fromListFL (Focus 0) ['a'..'c']
-- >>> lookupFL 100 fl
-- Nothing
-- >>> lookupFL (-1) fl
-- Nothing
--
-- Always returns 'Nothing' if the 'FocusList' is empty.
--
-- prop> lookupFL i emptyFL == Nothing
--
-- @O(log(min(i, n - i)))@ where @i@ is the index you want to look up, and @n@
-- is the length of the 'FocusList'.
lookupFL
  :: Int  -- ^ Index to lookup.
  -> FocusList a
  -> Maybe a
lookupFL i fl = S.lookup i (fl ^. lensFocusList)

-- | Insert a new value into the 'FocusList'.  The 'Focus' of the list is
-- changed appropriately.
--
-- Inserting an element into an empyt 'FocusList' will set the 'Focus' on
-- that element.
--
-- >>> insertFL 0 "hello" emptyFL
-- FocusList (Focus 0) ["hello"]
--
-- The 'Focus' will not be changed if you insert a new element after the
-- current 'Focus'.
--
-- >>> insertFL 1 "hello" (singletonFL "bye")
-- FocusList (Focus 0) ["bye","hello"]
--
-- The 'Focus' will be bumped up by one if you insert a new element before
-- the current 'Focus'.
--
-- >>> insertFL 0 "hello" (singletonFL "bye")
-- FocusList (Focus 1) ["hello","bye"]
--
-- Behaves like @Data.Sequence.'Data.Sequence.insertAt'@. If the index is out of bounds, it will be
-- inserted at the nearest available index
--
-- >>> insertFL 100 "hello" emptyFL
-- FocusList (Focus 0) ["hello"]
--
-- >>> insertFL 100 "bye" (singletonFL "hello")
-- FocusList (Focus 0) ["hello","bye"]
--
-- >>> insertFL (-1) "bye" (singletonFL "hello")
-- FocusList (Focus 1) ["bye","hello"]
--
-- @O(log(min(i, n - i)))@ where @i@ is the index you want to insert at, and @n@
-- is the length of the 'FocusList'.
insertFL
  :: Int  -- ^ The index at which to insert the new element.
  -> a    -- ^ The new element.
  -> FocusList a
  -> FocusList a
insertFL _ a FocusList {focusListFocus = NoFocus} = singletonFL a
insertFL i a fl@FocusList{focusListFocus = Focus focus, focusList = fls} =
  if i > focus
    then
      fl
        { focusList = S.insertAt i a fls
        }
    else
      fl
        { focusList = S.insertAt i a fls
        , focusListFocus = Focus $ focus + 1
        }

-- | Remove an element from a 'FocusList'.
--
-- If the element to remove is not the 'Focus', then update the 'Focus'
-- accordingly.
--
-- For example, if the 'Focus' is on index 1, and we have removed index 2, then
-- the focus is not affected, so it is not changed.
--
-- >>> let focusList = unsafeFromListFL (Focus 1) ["cat","goat","dog","hello"]
-- >>> removeFL 2 focusList
-- Just (FocusList (Focus 1) ["cat","goat","hello"])
--
-- If the 'Focus' is on index 2 and we have removed index 1, then the 'Focus'
-- will be moved back one element to set to index 1.
--
-- >>> let focusList = unsafeFromListFL (Focus 2) ["cat","goat","dog","hello"]
-- >>> removeFL 1 focusList
-- Just (FocusList (Focus 1) ["cat","dog","hello"])
--
-- If we remove the 'Focus', then the next item is set to have the 'Focus'.
--
-- >>> let focusList = unsafeFromListFL (Focus 0) ["cat","goat","dog","hello"]
-- >>> removeFL 0 focusList
-- Just (FocusList (Focus 0) ["goat","dog","hello"])
--
-- If the element to remove is the only element in the list, then the 'Focus'
-- will be set to 'NoFocus'.
--
-- >>> let focusList = unsafeFromListFL (Focus 0) ["hello"]
-- >>> removeFL 0 focusList
-- Just (FocusList NoFocus [])
--
-- If the 'Int' for the index to remove is either less than 0 or greater then
-- the length of the list, then 'Nothing' is returned.
--
-- >>> let focusList = unsafeFromListFL (Focus 0) ["hello"]
-- >>> removeFL (-1) focusList
-- Nothing
--
-- >>> let focusList = unsafeFromListFL (Focus 1) ["hello","bye","cat"]
-- >>> removeFL 3 focusList
-- Nothing
--
-- If the 'FocusList' passed in is 'Empty', then 'Nothing' is returned.
--
-- >>> removeFL 0 emptyFL
-- Nothing
--
-- @O(log(min(i, n - i)))@ where @i@ is index of the element to remove, and @n@
-- is the length of the 'FocusList'.
removeFL
  :: Int          -- ^ Index of the element to remove from the 'FocusList'.
  -> FocusList a  -- ^ The 'FocusList' to remove an element from.
  -> Maybe (FocusList a)
removeFL i fl@FocusList{focusList = fls}
  | i < 0 || i >= (lengthFL fl) || isEmptyFL fl =
    -- Return Nothing if the removal position is out of bounds.
    Nothing
  | lengthFL fl == 1 =
    -- Return an empty focus list if there is currently only one element
    Just emptyFL
  | otherwise =
    let newFL = fl {focusList = S.deleteAt i fls}
        focus = unsafeGetFocusFL fl
    in
    if focus >= i && focus /= 0
      then Just $ newFL & lensFocusListFocus . _Focus -~ 1
      else Just newFL

-- | Find the index of the first element in the 'FocusList'.
--
-- >>> let Just fl = fromListFL (Focus 1) ["hello", "bye", "tree"]
-- >>> indexOfFL "hello" fl
-- Just 0
--
-- If more than one element exists, then return the index of the first one.
--
-- >>> let Just fl = fromListFL (Focus 1) ["dog", "cat", "cat"]
-- >>> indexOfFL "cat" fl
-- Just 1
--
-- If the element doesn't exist, then return 'Nothing'
--
-- >>> let Just fl = fromListFL (Focus 1) ["foo", "bar", "baz"]
-- >>> indexOfFL "hogehoge" fl
-- Nothing
indexOfFL :: Eq a => a -> FocusList a -> Maybe Int
indexOfFL a FocusList{focusList = fls} =
  S.elemIndexL a fls

-- | Delete an element from a 'FocusList'.
--
-- >>> let Just fl = fromListFL (Focus 0) ["hello", "bye", "tree"]
-- >>> deleteFL "bye" fl
-- FocusList (Focus 0) ["hello","tree"]
--
-- The focus will be updated if an item before it is deleted.
--
-- >>> let Just fl = fromListFL (Focus 1) ["hello", "bye", "tree"]
-- >>> deleteFL "hello" fl
-- FocusList (Focus 0) ["bye","tree"]
--
-- If there are multiple matching elements in the 'FocusList', remove them all.
--
-- >>> let Just fl = fromListFL (Focus 0) ["hello", "bye", "bye"]
-- >>> deleteFL "bye" fl
-- FocusList (Focus 0) ["hello"]
--
-- If there are no matching elements, return the original 'FocusList'.
--
-- >>> let Just fl = fromListFL (Focus 2) ["hello", "good", "bye"]
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
--
-- @O(1)@
setFocusFL :: Int -> FocusList a -> Maybe (FocusList a)
setFocusFL i fl
  -- Can't set a 'Focus' for an empty 'FocusList'.
  | isEmptyFL fl = Nothing
  | otherwise =
    let len = lengthFL fl
    in
    if i < 0 || i >= len
      then Nothing
      else Just $ fl & lensFocusListFocus . _Focus .~ i

-- | Update the 'Focus' for a 'FocusList' and get the new focused element.
--
-- >>> updateFocusFL 1 =<< fromListFL (Focus 2) ["hello","bye","dog","cat"]
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
-- >>> updateFocusFL (-1) =<< fromListFL (Focus 2) ["hello","bye","dog","cat"]
-- Nothing
--
-- >>> updateFocusFL 4 =<< fromListFL (Focus 2) ["hello","bye","dog","cat"]
-- Nothing
--
-- @O(log(min(i, n - i)))@ where @i@ is the new index to put the 'Focus' on,
-- and @n@ -- is the length of the 'FocusList'.
updateFocusFL
  :: Int  -- ^ The new index to put the 'Focus' on.
  -> FocusList a
  -> Maybe (a, FocusList a)
  -- ^ A tuple of the new element that gets the 'Focus', and the new
  -- 'FocusList'.
updateFocusFL i fl
  | isEmptyFL fl = Nothing
  | otherwise =
    let len = lengthFL fl
    in
    if i < 0 || i >= len
      then Nothing
      else
        let newFL = fl & lensFocusListFocus . _Focus .~ i
        in Just (unsafeGetFocusItemFL newFL, newFL)

-- | Find a value in a 'FocusList'.  Similar to @Data.List.'Data.List.find'@.
--
-- >>> let Just fl = fromListFL (Focus 1) ["hello", "bye", "tree"]
-- >>> findFL (\a -> a == "hello") fl
-- Just "hello"
--
-- This will only find the first value.
--
-- >>> let Just fl = fromListFL (Focus 0) ["hello", "bye", "bye"]
-- >>> findFL (\a -> a == "bye") fl
-- Just "bye"
--
-- If no values match the comparison, this will return 'Nothing'.
--
-- >>> let Just fl = fromListFL (Focus 1) ["hello", "bye", "parrot"]
-- >>> findFL (\a -> a == "ball") fl
-- Nothing
--
-- @O(n)@ where @n@ is the length of the 'FocusList'.
findFL :: (a -> Bool) -> FocusList a -> Maybe (a)
findFL p fl =
  let fls = fl ^. lensFocusList
  in find p fls

-- | Move an existing item in a 'FocusList' to a new index.
--
-- The 'Focus' gets updated appropriately when moving items.
--
-- >>> let Just fl = fromListFL (Focus 1) ["hello", "bye", "parrot"]
-- >>> moveFromToFL 0 1 fl
-- Just (FocusList (Focus 0) ["bye","hello","parrot"])
--
-- The 'Focus' may not get updated if it is not involved.
--
-- >>> let Just fl = fromListFL (Focus 0) ["hello", "bye", "parrot"]
-- >>> moveFromToFL 1 2 fl
-- Just (FocusList (Focus 0) ["hello","parrot","bye"])
--
-- If the element with the 'Focus' is moved, then the 'Focus' will be updated
-- appropriately.
--
-- >>> let Just fl = fromListFL (Focus 2) ["hello", "bye", "parrot"]
-- >>> moveFromToFL 2 0 fl
-- Just (FocusList (Focus 0) ["parrot","hello","bye"])
--
-- If the index of the item to move is out bounds, then 'Nothing' will be returned.
--
-- >>> let Just fl = fromListFL (Focus 2) ["hello", "bye", "parrot"]
-- >>> moveFromToFL 3 0 fl
-- Nothing
--
-- If the new index is out of bounds, then 'Nothing' wil be returned.
--
-- >>> let Just fl = fromListFL (Focus 2) ["hello", "bye", "parrot"]
-- >>> moveFromToFL 1 (-1) fl
-- Nothing
--
-- @O(log n)@ where @n@ is the length of the 'FocusList'.
moveFromToFL
  :: Show a
  => Int  -- ^ Index of the item to move.
  -> Int  -- ^ New index for the item.
  -> FocusList a
  -> Maybe (FocusList a)
moveFromToFL oldPos newPos fl
  | oldPos < 0 || oldPos >= length fl = Nothing
  | newPos < 0 || newPos >= length fl = Nothing
  | otherwise =
    let oldFocus = fl ^. lensFocusListFocus
    in
    case lookupFL oldPos fl of
      Nothing -> error "moveFromToFL should have been able to lookup the item"
      Just item ->
        case removeFL oldPos fl of
          Nothing -> error "moveFromToFL should have been able to remove old position"
          Just flAfterRemove ->
            let flAfterInsert = insertFL newPos item flAfterRemove in
                if Focus oldPos == oldFocus
                  then
                    case setFocusFL newPos flAfterInsert of
                      Nothing -> error "moveFromToFL should have been able to reset the focus"
                      Just flWithUpdatedFocus -> Just flWithUpdatedFocus
                  else Just flAfterInsert

-- | Intersperse a new element between existing elements in the 'FocusList'.
--
-- >>> let Just fl = fromListFL (Focus 0) ["hello", "bye", "cat"]
-- >>> intersperseFL "foo" fl
-- FocusList (Focus 0) ["hello","foo","bye","foo","cat"]
--
-- The 'Focus' is updated accordingly.
--
-- >>> let Just fl = fromListFL (Focus 2) ["hello", "bye", "cat", "goat"]
-- >>> intersperseFL "foo" fl
-- FocusList (Focus 4) ["hello","foo","bye","foo","cat","foo","goat"]
--
-- The item with the 'Focus' should never change after calling 'intersperseFL'.
--
-- prop> getFocusItemFL (fl :: FocusList Int) == getFocusItemFL (intersperseFL a fl)
--
-- 'intersperseFL' should not have any effect on a 'FocusList' with less than
-- two items.
--
-- prop> emptyFL == intersperseFL x emptyFL
-- prop> singletonFL a == intersperseFL x (singletonFL a)
--
-- @O(n)@ where @n@ is the length of the 'FocusList'.
intersperseFL :: a -> FocusList a -> FocusList a
intersperseFL _ FocusList{focusListFocus = NoFocus} = emptyFL
intersperseFL a FocusList{focusList = fls, focusListFocus = Focus foc} =
  let newFLS = intersperse a fls
  in
  FocusList
    { focusList = newFLS
    , focusListFocus = Focus (foc * 2)
    }

-- | Reverse a 'FocusList'.  The 'Focus' is updated accordingly.
--
-- >>> let Just fl = fromListFL (Focus 0) ["hello", "bye", "cat"]
-- >>> reverseFL fl
-- FocusList (Focus 2) ["cat","bye","hello"]
--
-- >>> let Just fl = fromListFL (Focus 2) ["hello", "bye", "cat", "goat"]
-- >>> reverseFL fl
-- FocusList (Focus 1) ["goat","cat","bye","hello"]
--
-- The item with the 'Focus' should never change after calling 'intersperseFL'.
--
-- prop> getFocusItemFL (fl :: FocusList Int) == getFocusItemFL (reverseFL fl)
--
-- Reversing twice should not change anything.
--
-- prop> (fl :: FocusList Int) == reverseFL (reverseFL fl)
--
-- Reversing empty lists and single lists should not do anything.
--
-- prop> emptyFL == reverseFL emptyFL
-- prop> singletonFL a == reverseFL (singletonFL a)
--
-- @O(n)@ where @n@ is the length of the 'FocusList'.
reverseFL :: FocusList a -> FocusList a
reverseFL FocusList{focusListFocus = NoFocus} = emptyFL
reverseFL FocusList{focusList = fls, focusListFocus = Focus foc} =
  let newFLS = reverse fls
      newFLSLen = length newFLS
  in
  FocusList
    { focusList = newFLS
    , focusListFocus = Focus (newFLSLen - foc - 1)
    }

-- | Sort a 'FocusList'.
--
-- The 'Focus' will stay with the element that has the 'Focus'.
--
-- >>> let Just fl = fromListFL (Focus 2) ["b", "c", "a"]
-- >>> sortByFL compare fl
-- FocusList (Focus 0) ["a","b","c"]
--
-- Nothing will happen if you try to sort an empty 'FocusList', or a
-- 'FocusList' with only one element.
--
-- prop> emptyFL == sortByFL compare emptyFL
-- prop> singletonFL a == sortByFL compare (singletonFL a)
--
-- The element with the 'Focus' should be the same before and after sorting.
--
-- prop> getFocusItemFL (fl :: FocusList Int) == getFocusItemFL (sortByFL compare fl)
--
-- Sorting a 'FocusList' and getting the underlying 'Seq' should be the same as
-- getting the underlying 'Seq' and then sorting it.
--
-- prop> toSeqFL (sortByFL compare (fl :: FocusList Int)) == sortBy compare (toSeqFL fl)
--
-- __WARNING__: The computational complexity for this is very bad. It should be
-- able to be done in @O(n * log n)@, but the current implementation is
-- @O(n^2)@ (or worse), where @n@ is the length of the 'FocusList'.  This
-- function could be implemented the same way
-- @Data.Sequence.'Data.Sequence.sortBy'@ is implemented.  However, a small
-- change needs to be added to that function to keep track of the 'Focus' in
-- the 'FocusList' and make sure it gets updated properly.  If you're
-- interested in fixing this, please send a PR.
sortByFL
  :: forall a
   . (a -> a -> Ordering) -- ^ The function to use to compare elements.
  -> FocusList a
  -> FocusList a
sortByFL _ FocusList{focusListFocus = NoFocus} = emptyFL
sortByFL cmpFunc FocusList{focusList = fls, focusListFocus = Focus foc} =
  let (res, maybeNewFoc) = go fls (Just foc)
  in
  case maybeNewFoc of
    Nothing -> error "sortByFL: A sequence should never lose its focus."
    Just newFoc ->
      FocusList
        { focusList = res
        , focusListFocus = Focus newFoc
        }
  where
    go
      :: S.Seq a -- ^ The sequence that needs to be sorted.
      -> Maybe Int
         -- ^ Whether or not we are tracking a 'Focus' that needs to be updated.
      -> (S.Seq a, Maybe Int)
    -- Trying to sort an empty sequence with a 'Focus'.  This should never happen.
    go Empty (Just _) =
      error "sortByFL: go: this should never happen, sort empty with focus."
    -- Trying to sort an empty sequence.
    go Empty Nothing = (Empty, Nothing)
    -- Trying to sort a non-empty sequence with no focus.
    go (a :<| as) Nothing =
      let res = go as Nothing
      in
      case res of
        (_, Just _) -> error "sortByFL: go: this should never happen, no focus case"
        (Empty, Nothing) -> (a :<| Empty, Nothing)
        (b :<| bs, Nothing) ->
          case cmpFunc a b of
            LT -> (a :<| b :<| bs, Nothing)
            EQ -> (a :<| b :<| bs, Nothing)
            GT -> (b :<| fst (go (a :<| bs) Nothing), Nothing)
    -- Trying to sort a non-empty sequence with the top element having the focus.
    go (a :<| as) (Just 0) =
      let res = go as Nothing
      in
      case res of
        (_, Just _) -> error "sortByFL: go: this should never happen, top elem has focus case"
        (Empty, Nothing) -> (a :<| Empty, Just 0)
        (b :<| bs, Nothing) ->
          case cmpFunc a b of
            LT -> (a :<| b :<| bs, Just 0)
            EQ -> (a :<| b :<| bs, Just 0)
            GT ->
              let (newSeq, maybeNewFoc) = go (a :<| bs) (Just 0)
              in
              case maybeNewFoc of
                Nothing -> error "sortByFL: go: this should never happen, lost the focus"
                Just newFoc -> (b :<| newSeq, Just (newFoc + 1))
    -- Trying to sort a non-empty sequence where some element other than the
    -- top element has the focus.
    go (a :<| as) (Just n) =
      let res = go as (Just (n - 1))
      in
      case res of
        (_, Nothing) -> error "sortByFL: go: this should never happen, no focus"
        (Empty, Just _) -> error "sortByFL: go: this should never happen, focus but no elems"
        (b :<| bs, Just newFoc) ->
          case cmpFunc a b of
            LT -> (a :<| b :<| bs, Just (newFoc + 1))
            EQ -> (a :<| b :<| bs, Just (newFoc + 1))
            GT ->
              case newFoc of
                0 -> (b :<| fst (go (a :<| bs) Nothing), Just 0)
                gt0 ->
                  let (newSeq, maybeNewFoc') = go (a :<| bs) (Just gt0)
                  in
                  case maybeNewFoc' of
                    Nothing -> error "sortByFL: go: this should never happen, lost the focus again"
                    Just newFoc' -> (b :<| newSeq, Just (newFoc' + 1))
