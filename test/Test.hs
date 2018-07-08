
module Main where

import Termonad.Prelude hiding (assert)

import Control.Lens ((^.))
import Data.Functor.Classes (Eq1)
import Hedgehog
  ( Callback(Ensure, Require, Update)
  , Command(Command)
  , Concrete(Concrete)
  , HTraversable(htraverse)
  , MonadGen
  , MonadTest
  , Property
  , Symbolic
  , Test
  , Var(Var)
  , (===)
  , annotate
  , assert
  , concrete
  , executeSequential
  , failure
  , forAll
  , property
  )
import Hedgehog.Gen (ascii, int, sequential, string)
import Hedgehog.Range (constant, linear)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Termonad.FocusList
  ( FocusList
  , debugFL
  , deleteFL
  , emptyFL
  , indexOfFL
  , insertFL
  , invariantFL
  , isEmptyFL
  , lensFocusListLen
  , lookupFL
  , removeFL
  )

main :: IO ()
main = do
  tests <- testsIO
  defaultMain tests

testsIO :: IO TestTree
testsIO = do
  pure $
    testGroup
      "tests"
      [ testProperty "foo" foo
      ]

foo :: Property
foo =
  property $ do
    actions <-
      forAll $ do
        sequential
          (linear 1 200)
          initialState
          [ insertFLCommand
          , removeFLCommand
          , deleteFLCommand
          ]
    traceShowM actions
    executeSequential initialState actions

-----------
-- State --
-----------

newtype State a (v :: * -> *) = State { unState :: FocusList (Var a v) } deriving (Eq, Show)

initialState :: State a v
initialState = State emptyFL

ensureInvariants :: State String Concrete -> State String Concrete -> a -> b -> Test ()
ensureInvariants (State startingFL) (State endingFL) _ _ = do
  assert (invariantFL startingFL)
  annotate (debugFL endingFL)
  assert (invariantFL endingFL)

--------------
-- InsertFL --
--------------

data InsertFL a v = InsertFL !(FocusList (Var a v)) !Int !a deriving (Eq, Show)

instance HTraversable (InsertFL x) where
  htraverse :: forall f g h. Applicative f => (forall a. g a -> f (h a)) -> InsertFL x g -> f (InsertFL x h)
  htraverse func (InsertFL fl newKey newVal) = InsertFL <$> traverse go fl <*> pure newKey <*> pure newVal
    where
      go :: forall a. Var a g -> f (Var a h)
      go var = htraverse func var

insertFLCommand :: forall n m. (MonadGen n, MonadTest m) => Command n m (State String)
insertFLCommand =
  Command
    generator
    execute
    [ Update update
    , Require require
    , Ensure ensureInvariants
    , Ensure ensureStringInFL
    ]
  where
    generator :: State String Symbolic -> Maybe (n (InsertFL String Symbolic))
    generator (State fl) =
      Just $ do
        let len = fl ^. lensFocusListLen
        newKey <- int $ constant 0 len
        newVal <- string (constant 0 25) ascii
        pure (InsertFL fl newKey newVal)

    execute :: InsertFL String Concrete -> m String
    execute (InsertFL fl newKey newVal) =
      case insertFL newKey (Var (Concrete newVal)) fl of
        Nothing -> do
          annotate "Failed to insert a value into the FocusList"
          failure
        Just _ -> pure newVal

    require :: State String Symbolic -> InsertFL String v -> Bool
    require (State fl) (InsertFL _ newKey _) =
      let len = fl ^. lensFocusListLen
      in newKey <= len

    update :: forall v. State String v -> InsertFL String v -> Var String v -> State String v
    update (State fl) (InsertFL _ newKey _) newVal =
      case insertFL newKey newVal fl of
        Nothing -> error "insertFLCommand, update: Failed to insert a value into the FocusList, even though we should be able to"
        Just newFL -> State newFL

    ensureStringInFL :: State String Concrete -> State String Concrete -> InsertFL String Concrete -> String -> Test ()
    ensureStringInFL _ (State endingFL) (InsertFL _ newKey newVal) _ =
      let maybeVal = lookupFL newKey endingFL
      in
      case maybeVal of
        Nothing -> do
          annotate "Couldn't find inserted value in FocusList"
          failure
        Just val -> val === Var (Concrete newVal)

--------------
-- RemoveFL --
--------------

data RemoveFL a v = RemoveFL !(FocusList (Var a v)) !Int deriving (Eq, Show)

instance HTraversable (RemoveFL x) where
  htraverse :: forall f g h. Applicative f => (forall a. g a -> f (h a)) -> RemoveFL x g -> f (RemoveFL x h)
  htraverse func (RemoveFL fl keyToRemove) = RemoveFL <$> traverse go fl <*> pure keyToRemove
    where
      go :: forall a. Var a g -> f (Var a h)
      go var = htraverse func var

removeFLCommand :: forall n m. (MonadGen n, MonadTest m) => Command n m (State String)
removeFLCommand =
  Command
    generator
    execute
    [ Update update
    , Require require
    , Ensure ensureInvariants
    , Ensure ensureActuallyRemoved
    ]
  where
    generator :: State String Symbolic -> Maybe (n (RemoveFL String Symbolic))
    generator (State fl) =
      if isEmptyFL fl
        then Nothing
        else
          Just $ do
            let len = fl ^. lensFocusListLen
            keyToRemove <- int $ constant 0 (len - 1)
            pure (RemoveFL fl keyToRemove)

    execute :: RemoveFL String Concrete -> m ()
    execute (RemoveFL fl keyToRemove) = pure ()

    require :: State String Symbolic -> RemoveFL String v -> Bool
    require (State fl) (RemoveFL _ keyToRemove) =
      let len = fl ^. lensFocusListLen
      in not (isEmptyFL fl) && keyToRemove < len

    update :: forall a v. State String v -> RemoveFL String v -> Var a v -> State String v
    update (State fl) (RemoveFL _ keyToRemove) _ =
      case removeFL keyToRemove fl of
        Nothing -> State fl
        Just newFL -> State newFL

    ensureActuallyRemoved :: State String Concrete -> State String Concrete -> RemoveFL String Concrete -> a -> Test ()
    ensureActuallyRemoved (State startingFL) (State endingFL) (RemoveFL _ _) _ =
      if isEmptyFL startingFL
        then startingFL === endingFL
        else do
          let startingFLLen = startingFL ^. lensFocusListLen
              endingFLLen = endingFL ^. lensFocusListLen
          annotate (debugFL startingFL)
          annotate (debugFL endingFL)
          startingFLLen - 1 === endingFLLen

--------------
-- DeleteFL --
--------------

data DeleteFL a v = DeleteFL !(FocusList (Var a v)) !(Var a v) deriving (Eq, Show)

instance HTraversable (DeleteFL x) where
  htraverse :: forall f g h. Applicative f => (forall a. g a -> f (h a)) -> DeleteFL x g -> f (DeleteFL x h)
  htraverse func (DeleteFL fl itemToRemove) = DeleteFL <$> traverse go fl <*> htraverse func itemToRemove
    where
      go :: forall a. Var a g -> f (Var a h)
      go var = htraverse func var

deleteFLCommand :: forall n m. (MonadGen n, MonadTest m) => Command n m (State String)
deleteFLCommand =
  Command
    generator
    execute
    [ Update update
    , Require require
    , Ensure ensureInvariants
    , Ensure ensureLenLess
    , Ensure ensureActuallyRemoved
    ]
  where
    generator :: State String Symbolic -> Maybe (n (DeleteFL String Symbolic))
    generator (State fl) =
      let len = fl ^. lensFocusListLen
      in
      if len == 0
          then Nothing
          else
            Just $ do
              keyToGetItem <- int $ constant 0 (if len == 0 then 0 else len - 1)
              let maybeItemToDelete = lookupFL keyToGetItem fl
              case maybeItemToDelete of
                Just itemToDelete -> pure (DeleteFL fl itemToDelete)
                Nothing ->
                  let msg =
                        "Couldn't find item at index even though it should exist." <>
                        "\nlen: " <>
                        show len <>
                        "\nkeyToGetItem: " <>
                        show keyToGetItem
                  in error msg

    execute :: DeleteFL String Concrete -> m String
    execute (DeleteFL fl itemToDelete) = pure $ concrete itemToDelete

    require :: State String Symbolic -> DeleteFL String Symbolic -> Bool
    require (State fl) (DeleteFL _ itemToRemove) =
      isEmptyFL fl || isJust (indexOfFL itemToRemove fl)

    update :: forall v a. Eq1 v => State String v -> DeleteFL String v -> Var a v -> State String v
    update (State fl) (DeleteFL _ itemToRemove) _ = State $ deleteFL itemToRemove fl

    ensureLenLess :: State String Concrete -> State String Concrete -> DeleteFL String Concrete -> a -> Test ()
    ensureLenLess (State startingFL) (State endingFL) (DeleteFL _ itemToRemove) _ =
      case indexOfFL itemToRemove startingFL of
        Nothing -> startingFL === endingFL
        Just _ -> do
          let startingFLLen = startingFL ^. lensFocusListLen
              endingFLLen = endingFL ^. lensFocusListLen
          annotate (debugFL startingFL)
          annotate (debugFL endingFL)
          assert $ startingFLLen > endingFLLen

    ensureActuallyRemoved :: State String Concrete -> State String Concrete -> DeleteFL String Concrete -> String -> Test ()
    ensureActuallyRemoved _ (State endingFL) _ itemDeleted =
      indexOfFL (Var (Concrete itemDeleted)) endingFL === Nothing
