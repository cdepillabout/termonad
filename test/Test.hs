
module Main where

import Termonad.Prelude hiding (assert)

import Control.Lens ((^.))
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
  , executeSequential
  , failure
  , forAll
  , property
  )
import Hedgehog.Gen (ascii, int, sequential, string)
import Hedgehog.Range (linear)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Termonad.FocusList
  ( FocusList
  , debugFL
  , emptyFL
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
          (linear 1 100)
          initialState
          [ insertFLCommand
          , removeFLCommand
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
        newKey <- int $ linear 0 len
        newVal <- string (linear 0 25) ascii
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
      Just $ do
        let len = fl ^. lensFocusListLen
        keyToRemove <- int $ linear 0 len
        pure (RemoveFL fl keyToRemove)

    execute :: RemoveFL String Concrete -> m ()
    execute (RemoveFL fl keyToRemove) =
      case removeFL keyToRemove fl of
        Nothing -> pure () -- annotate "Can't remove from an empty focus list"
        Just _ -> pure ()

    require :: State String Symbolic -> RemoveFL String v -> Bool
    require (State fl) (RemoveFL _ keyToRemove) =
      let len = fl ^. lensFocusListLen
      in isEmptyFL fl || keyToRemove < len

    update :: forall v. State String v -> RemoveFL String v -> Var () v -> State String v
    update (State fl) (RemoveFL _ keyToRemove) _ =
      case removeFL keyToRemove fl of
        Nothing -> State fl
        Just newFL -> State newFL

    ensureActuallyRemoved :: State String Concrete -> State String Concrete -> RemoveFL String Concrete -> () -> Test ()
    ensureActuallyRemoved (State startingFL) (State endingFL) (RemoveFL _ _) () =
      if isEmptyFL startingFL
        then startingFL === endingFL
        else
          let startingFLLen = startingFL ^. lensFocusListLen
              endingFLLen = endingFL ^. lensFocusListLen
          in startingFLLen - 1 === endingFLLen
