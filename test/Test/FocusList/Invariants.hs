
module Test.FocusList.Invariants where

import Termonad.Prelude

import Control.Lens ((^.))
import Hedgehog
  ( Gen
  , Property
  , PropertyT
  , annotate
  , annotateShow
  , failure
  , forAll
  , property
  , success
  )
import Hedgehog.Gen (alphaNum, choice, int, string)
import Hedgehog.Range (constant, linear)

import Termonad.FocusList
  ( FocusList
  , debugFL
  , deleteFL
  , emptyFL
  , insertFL
  , invariantFL
  , isEmptyFL
  , lengthFL
  , lookupFL
  , removeFL
  )

testInvariantsInFocusList :: Property
testInvariantsInFocusList =
  property $ do
    numOfActions <- forAll $ int (linear 1 200)
    let initialState = emptyFL
    let strGen = string (constant 0 25) alphaNum
    -- traceM "----------------------------------"
    -- traceM $ "starting bar, numOfActions: " <> show numOfActions
    runActions numOfActions strGen initialState

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
      let len = lengthFL fl
      key <- int $ constant 0 len
      val <- valGen
      pure $ InsertFL key val

genRemoveFL :: FocusList a -> Maybe (Gen (Action a))
genRemoveFL fl
  | isEmptyFL fl = Nothing
  | otherwise = Just $ do
      let len = lengthFL fl
      keyToRemove <- int $ constant 0 (len - 1)
      pure $ RemoveFL keyToRemove

genDeleteFL :: Show a => FocusList a -> Maybe (Gen (Action a))
genDeleteFL fl
  | isEmptyFL fl = Nothing
  | otherwise = Just $ do
      let len = lengthFL fl
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
          , genDeleteFL fl
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
