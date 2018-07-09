
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
import Hedgehog.Gen (alphaNum, choice, int, sequential, string)
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
      [ testProperty "invariants in FocusList" testInvariantsInFocusList
      ]

testInvariantsInFocusList :: Property
testInvariantsInFocusList =
  property $ do
    numOfActions <- forAll $ int (linear 1 200)
    let initialState = emptyFL
    let strGen = string (constant 0 25) alphaNum
    -- traceM "----------------------------------"
    -- traceM $ "starting bar, numOfActions: " <> show numOfActions
    runActions numOfActions strGen emptyFL

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
genRemoveFL fl
  | isEmptyFL fl = Nothing
  | otherwise = Just $ do
      let len = fl ^. lensFocusListLen
      keyToRemove <- int $ constant 0 (len - 1)
      pure $ RemoveFL keyToRemove

genDeleteFL :: Show a => Gen a -> FocusList a -> Maybe (Gen (Action a))
genDeleteFL valGen fl
  | isEmptyFL fl = Nothing
  | otherwise = Just $ do
      let len = fl ^. lensFocusListLen
      keyForItemToDelete <- int $ constant 0 (len - 1)
      let maybeItemToDelete = lookupFL keyForItemToDelete fl
      case maybeItemToDelete of
        Nothing ->
          let msg =
                "Could not find item in focuslist even though " <>
                "it should be there." <>
                "\nkey: " <>
                show keyForItemToDelete <>
                "\nfocus list: " <>
                debugFL fl
          in error msg
        Just item -> pure $ DeleteFL item

generateAction :: Show a => Gen a -> FocusList a -> Gen (Action a)
generateAction valGen fl = do
  let generators =
        catMaybes
          [ genInsertFL valGen fl
          , genRemoveFL fl
          , genDeleteFL valGen fl
          ]
  case generators of
    [] ->
      let msg =
            "No generators available for fl:\n" <>
            debugFL fl
      in error msg
    _ -> do
      choice generators

performAction :: Eq a => FocusList a -> Action a -> Maybe (FocusList a)
performAction fl (InsertFL key val) = insertFL key val fl
performAction fl (RemoveFL keyToRemove) = removeFL keyToRemove fl
performAction fl (DeleteFL valToDelete) = Just $ deleteFL valToDelete fl

runActions :: (Eq a, Monad m, Show a) => Int -> Gen a -> FocusList a -> PropertyT m ()
runActions i valGen startingFL
  | i <= 0 = success
  | otherwise = do
    action <- forAll $ generateAction valGen startingFL
    -- traceM $ "runActions, startingFL: " <> show startingFL
    -- traceM $ "runActions, action: " <> show action
    let maybeEndingFL = performAction startingFL action
    case maybeEndingFL of
      Nothing -> do
        annotate "Failed to perform action."
        annotateShow startingFL
        annotateShow action
        failure
      Just endingFL ->
        if invariantFL endingFL
          then runActions (i - 1) valGen endingFL
          else do
            annotate "Ending FocusList failed invariants."
            annotateShow startingFL
            annotateShow action
            annotateShow endingFL
            failure
