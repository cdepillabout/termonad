{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Termonad.FocusList where

import Termonad.Prelude

import Control.Lens
import qualified Data.Foldable as Foldable
import qualified Data.Sequence as S
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
  , focusList :: !(S.Seq  a)
  } deriving (Eq, Functor, Generic)

$(makeLensesFor
    [ ("focusListFocus", "lensFocusListFocus")
    , ("focusList", "lensFocusList")
    ]
    ''FocusList
 )

instance Foldable FocusList where
  foldr f b (FocusList _ fls) = Foldable.foldr f b fls

instance Traversable FocusList where
  traverse :: Applicative f => (a -> f b) -> FocusList a -> f (FocusList b)
  traverse f (FocusList focus fls) = FocusList focus <$> traverse f fls

type instance Element (FocusList a) = a

instance MonoFunctor (FocusList a)

instance MonoFoldable (FocusList a)

instance MonoTraversable (FocusList a)

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
          pure $ unsafeFLFromList (Focus len) arbList

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

-- | Return the length of a 'FocusList'.
--
-- >>> let Just fl = fromListFL (Focus 2) ["hello", "bye", "parrot"]
-- >>> lengthFL fl
-- 3
lengthFL :: FocusList a -> Int
lengthFL = S.length . focusList

-- | This is an invariant that the 'FocusList' must always protect.
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
-- >>> unsafeFLFromList (Focus 1) [0..2]
-- FocusList (Focus 1) [0,1,2]
--
-- >>> unsafeFLFromList NoFocus []
-- FocusList NoFocus []
--
-- This allows you create a 'FocusList' that does not pass 'invariantFL'.
--
-- >>> unsafeFLFromList (Focus 100) [0..2]
-- FocusList (Focus 100) [0,1,2]
unsafeFLFromList :: Focus -> [a] -> FocusList a
unsafeFLFromList focus list =
  FocusList
    { focusListFocus = focus
    , focusList = S.fromList list
    }

focusItemGetter :: Getter (FocusList a) (Maybe a)
focusItemGetter = to getFLFocusItem

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

-- | Create a 'FocusList' with a single element.
--
-- >>> singletonFL "hello"
-- FocusList (Focus 0) ["hello"]
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
-- prop> (appendSetFocusFL fl a) ^. lensFocusListFocus /= fl ^. lensFocusListFocus
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
prependFL :: a -> FocusList a -> FocusList a
prependFL a fl@FocusList{ focusListFocus = focus, focusList = fls}  =
  case focus of
    NoFocus -> singletonFL a
    Focus i   -> fl {focusListFocus = Focus (i+1)
                    , focusList = a S.<| fls}

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
      let fls = fl ^. lensFocusList --fls = focus list sequence
      in
      case S.lookup i fls of
        Nothing ->
          error $
            "unsafeGetFLFocusItem: internal error, i (" <>
            show i <>
            ") doesnt exist in sequence"
        Just a -> a

getFLFocusItem :: FocusList a -> Maybe a
getFLFocusItem fl =
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
            "getFLFocusItem: internal error, i (" <>
            show i <>
            ") doesnt exist in sequence"
        Just a -> Just a

lookupFL :: Int -> FocusList a -> Maybe a
lookupFL i fl = S.lookup i (fl ^. lensFocusList)

-- | Insert a new value into the 'FocusList'.  The 'Focus' of the list is
-- changed appropriately.
--
-- >>> insertFL 0 "hello" emptyFL
-- FocusList (Focus 0) ["hello"]
--
-- >>> insertFL 0 "hello" (singletonFL "bye")
-- FocusList (Focus 1) ["hello","bye"]
--
-- >>> insertFL 1 "hello" (singletonFL "bye")
-- FocusList (Focus 0) ["bye","hello"]
--
-- Behaves like Data.Sequence.InsertAt, if the index is out of bounds, it will be
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
insertFL
  :: Int  -- ^ The index at which to insert the value.
  -> a
  -> FocusList a
  -> FocusList a
insertFL _ a FocusList {focusListFocus = NoFocus} = singletonFL a
insertFL i a fl@FocusList{focusListFocus = Focus focus, focusList = fls} =
  if i > focus
  then fl {focusList = S.insertAt i a fls}
  else fl {focusList = S.insertAt i a fls,
           focusListFocus = Focus $ focus + 1}

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
removeFL i fl@FocusList{focusList = fls}
  | i < 0 || i >= (lengthFL fl) || isEmptyFL fl =
    -- Return Nothing if the removal position is out of bounds.
    Nothing
  | lengthFL fl  == 1 =
    -- Return an empty focus list if there is currently only one element
    Just emptyFL
  | otherwise =
    let newFL = fl {focusList = S.deleteAt i fls}
        focus = unsafeGetFLFocus fl
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
updateFocusFL :: Int -> FocusList a -> Maybe (a, FocusList a)
updateFocusFL i fl
  | isEmptyFL fl = Nothing
  | otherwise =
    let len = lengthFL fl
    in
    if i < 0 || i >= len
      then Nothing
      else
        let newFL = fl & lensFocusListFocus . _Focus .~ i
        in Just (unsafeGetFLFocusItem newFL, newFL)

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
-- If the element with the 'Focus' is moved, then the 'Focus' will be updated appropriately.
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
moveFromToFL
  :: Show a => Int  -- ^ Index of the item to move.
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
