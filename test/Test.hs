
module Main where

import Termonad.Prelude hiding (assert)

import Control.Lens ((^.))
import Data.Functor.Classes (Eq1)
import Hedgehog
  ( Callback(Ensure, Require, Update)
  , Command(Command)
  , Concrete(Concrete)
  , Gen
  , HTraversable(htraverse)
  , MonadGen
  , MonadTest
  , Property
  , PropertyT
  , Symbolic
  , Test
  , Var(Var)
  , (===)
  , annotate
  , annotateShow
  , assert
  , concrete
  , executeSequential
  , failure
  , forAll
  , property
  , success
  , withShrinks
  )
import Hedgehog.Gen (ascii, choice, int, sequential, string)
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
      [ testProperty "bar" bar
      ]

bar :: Property
bar =
  property $ do
    numOfActions <- forAll $ int (linear 1 200)
    let initialState = emtpyFL
    runActions numOfActions emptyFL

data Action a
  = InsertFL Int a
  | RemoveFL Int
  | DeleteFL a
  deriving (Eq, Show)

genInsertFL :: Gen a -> FocusList a -> Maybe (Gen (Action a))
genInsertFL valGen fl
  | isEmptyFL fl = Just $ do
    val <- valGen
    pure $ InsertFL 0 val
  | otherwise = Just $ do
    let len = fl ^. lensFocusListLen
    key <- int $ constant 0 len
    val <- valGen
    pure $ InsertFL key val

genRemoveFL :: FocusList a -> Maybe (Gen (Action a))
genRemoveFL fl = Nothing

genDeleteFL :: Gen a -> FocusList a -> Maybe (Gen (Action a))
genDeleteFL valGen fl = Nothing

generateAction :: Gen a -> FocusList a -> Gen (Action a)
generateAction valGen fl = do
  let generators = catMaybes [genInsertFL, genRemoveFL, genDeleteFL]
  case generators of
    [] ->
      let msg =
            "No generators available for fl:\n" <>
            debugFL fl
      in error msg
    _ -> do
      choice generators

runActions :: Int -> FocusList a -> PropertyT m ()
runActions i fl
  | i <= 0 = success
  | otherwise = do
    action <- generateAction undefined fl
    undefined

--foo :: Property
--foo =
--  property $ do
--    actions <-
--      forAll $ do
--        sequential
--          (linear 1 200)
--          initialState
--          [ insertFLCommand
--          , removeFLCommand
--          , deleteFLCommand
--          ]
--    traceShowM actions
--    executeSequential initialState actions

-------------
---- State --
-------------

--newtype State a (v :: * -> *) = State { unState :: FocusList (Var a v) } deriving (Eq, Show)

--initialState :: State a v
--initialState = State emptyFL

--ensureInvariants :: State String Concrete -> State String Concrete -> a -> b -> Test ()
--ensureInvariants (State startingFL) (State endingFL) _ _ = do
--  assert (invariantFL startingFL)
--  annotate (debugFL endingFL)
--  assert (invariantFL endingFL)

----------------
---- InsertFL --
----------------

--data InsertFL a v = InsertFL !Int !a deriving (Eq, Show)

--instance HTraversable (InsertFL x) where
--  htraverse :: forall f g h. Applicative f => (forall a. g a -> f (h a)) -> InsertFL x g -> f (InsertFL x h)
--  htraverse _ (InsertFL newKey newVal) = pure (InsertFL newKey newVal)

--insertFLCommand :: forall n m. (MonadGen n, MonadTest m) => Command n m (State String)
--insertFLCommand =
--  Command
--    generator
--    execute
--    [ Update update
--    , Require require
--    , Ensure ensureInvariants
--    , Ensure ensureStringInFL
--    ]
--  where
--    generator :: State String Symbolic -> Maybe (n (InsertFL String Symbolic))
--    generator (State fl) =
--      Just $ do
--        let len = fl ^. lensFocusListLen
--        newKey <- int $ constant 0 len
--        newVal <- string (constant 0 25) ascii
--        pure (InsertFL newKey newVal)

--    execute :: InsertFL String Concrete -> m String
--    execute (InsertFL _ newVal) = pure newVal

--    require :: State String Symbolic -> InsertFL String Symbolic -> Bool
--    require (State fl) (InsertFL newKey _) =
--      let len = fl ^. lensFocusListLen
--      in
--      if len == 0
--        then newKey == 0
--        else newKey >= 0 && newKey <= len

--    update :: forall v. State String v -> InsertFL String v -> Var String v -> State String v
--    update (State fl) (InsertFL newKey newVal) newValVar =
--      case insertFL newKey newValVar fl of
--        Nothing ->
--          let msg =
--                "insertFLCommand, update: Failed to insert a value into " <>
--                "the FocusList, even though we should be able to" <>
--                  "\nnewKey: " <>
--                  show newKey <>
--                  "\nnewVal: " <>
--                  show newVal <>
--                  "\nfocus list len: " <>
--                  show (fl ^. lensFocusListLen)
--          in error msg
--        Just newFL -> State newFL

--    ensureStringInFL :: State String Concrete -> State String Concrete -> InsertFL String Concrete -> String -> Test ()
--    ensureStringInFL _ (State endingFL) (InsertFL newKey newVal) _ =
--      let maybeVal = lookupFL newKey endingFL
--      in
--      case maybeVal of
--        Nothing -> do
--          annotate (debugFL endingFL)
--          annotateShow newKey
--          annotateShow newVal
--          annotate "Couldn't find inserted value in FocusList"
--          failure
--        Just val -> val === Var (Concrete newVal)

----------------
---- RemoveFL --
----------------

--newtype RemoveFL a v = RemoveFL Int deriving (Eq, Show)

--instance HTraversable (RemoveFL x) where
--  htraverse :: forall f g h. Applicative f => (forall a. g a -> f (h a)) -> RemoveFL x g -> f (RemoveFL x h)
--  htraverse func (RemoveFL keyToRemove) = pure $ RemoveFL keyToRemove

--removeFLCommand :: forall n m. (MonadGen n, MonadTest m) => Command n m (State String)
--removeFLCommand =
--  Command
--    generator
--    execute
--    [ Update update
--    , Require require
--    , Ensure ensureInvariants
--    , Ensure ensureActuallyRemoved
--    ]
--  where
--    generator :: State String Symbolic -> Maybe (n (RemoveFL String Symbolic))
--    generator (State fl) =
--      if isEmptyFL fl
--        then Nothing
--        else
--          Just $ do
--            let len = fl ^. lensFocusListLen
--            keyToRemove <- int $ constant 0 (len - 1)
--            pure $ RemoveFL keyToRemove

--    execute :: RemoveFL String Concrete -> m ()
--    execute _ = pure ()

--    require :: State String Symbolic -> RemoveFL String v -> Bool
--    require (State fl) (RemoveFL keyToRemove) =
--      traceShow fl $ traceShow keyToRemove $
--      not (isEmptyFL fl) && keyToRemove < (fl ^. lensFocusListLen)

--    update :: forall a v. State String v -> RemoveFL String v -> Var a v -> State String v
--    update (State fl) (RemoveFL keyToRemove) _ =
--      case removeFL keyToRemove fl of
--        Nothing ->
--          let msg =
--                "removeFLCommand, update: Failed to remove a value from " <>
--                "the FocusList, even though we should be able to." <>
--                "\nkeyToRemove: " <>
--                show keyToRemove <>
--                "\nfocus list: " <>
--                debugFL (fmap (const "(can't show)") fl)
--          in error msg
--        Just newFL -> State newFL

--    ensureActuallyRemoved :: State String Concrete -> State String Concrete -> RemoveFL String Concrete -> a -> Test ()
--    ensureActuallyRemoved (State startingFL) (State endingFL) (RemoveFL keyToRemove) _ = do
--      let startingFLLen = startingFL ^. lensFocusListLen
--          endingFLLen = endingFL ^. lensFocusListLen
--      annotateShow keyToRemove
--      annotate (debugFL startingFL)
--      annotate (debugFL endingFL)
--      startingFLLen - 1 === endingFLLen

----------------
---- DeleteFL --
----------------

--data DeleteFL a v = DeleteFL !(Var a v) deriving (Eq, Show)

--instance HTraversable (DeleteFL x) where
--  htraverse :: forall f g h. Applicative f => (forall a. g a -> f (h a)) -> DeleteFL x g -> f (DeleteFL x h)
--  htraverse func (DeleteFL itemToRemove) = DeleteFL <$> htraverse func itemToRemove

--deleteFLCommand :: forall n m. (MonadGen n, MonadTest m) => Command n m (State String)
--deleteFLCommand =
--  Command
--    generator
--    execute
--    [ Update update
--    , Require require
--    , Ensure ensureInvariants
--    , Ensure ensureLenLess
--    , Ensure ensureActuallyRemoved
--    ]
--  where
--    generator :: State String Symbolic -> Maybe (n (DeleteFL String Symbolic))
--    generator (State fl) =
--      let len = fl ^. lensFocusListLen
--      in
--      if len == 0
--          then Nothing
--          else
--            Just $ do
--              keyToGetItem <- int $ constant 0 (len - 1)
--              let maybeItemToDelete = lookupFL keyToGetItem fl
--              case maybeItemToDelete of
--                Just itemToDelete -> pure (DeleteFL itemToDelete)
--                Nothing ->
--                  let msg =
--                        "Couldn't find item at index even though it should exist." <>
--                        "\nlen: " <>
--                        show len <>
--                        "\nkeyToGetItem: " <>
--                        show keyToGetItem
--                  in error msg

--    execute :: DeleteFL String Concrete -> m ()
--    execute _ = pure ()

--    require :: State String Symbolic -> DeleteFL String Symbolic -> Bool
--    require (State fl) (DeleteFL itemToRemove) =
--      isJust (indexOfFL itemToRemove fl)

--    update :: forall v a. Eq1 v => State String v -> DeleteFL String v -> Var a v -> State String v
--    update (State fl) (DeleteFL itemToRemove) _ = State $ deleteFL itemToRemove fl

--    ensureLenLess :: State String Concrete -> State String Concrete -> DeleteFL String Concrete -> a -> Test ()
--    ensureLenLess (State startingFL) (State endingFL) (DeleteFL itemToRemove) _ = do
--      let startingFLLen = startingFL ^. lensFocusListLen
--          endingFLLen = endingFL ^. lensFocusListLen
--      annotate (debugFL startingFL)
--      annotate (debugFL endingFL)
--      assert $ startingFLLen > endingFLLen

--    ensureActuallyRemoved :: State String Concrete -> State String Concrete -> DeleteFL String Concrete -> a -> Test ()
--    ensureActuallyRemoved _ (State endingFL) (DeleteFL itemToRemove) _ =
--      indexOfFL itemToRemove endingFL === Nothing
