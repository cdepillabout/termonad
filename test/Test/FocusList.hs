
module Test.FocusList where

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
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Termonad.FocusList
  ( FocusList
  , debugFL
  , deleteFL
  , emptyFL
  , insertFL
  , invariantFL
  , isEmptyFL
  , lensFocusListLen
  , lookupFL
  , removeFL
  )

import Test.FocusList.Invariants (testInvariantsInFocusList)

focusListTests :: TestTree
focusListTests = do
  testGroup
    "FocusList"
    [ testProperty "invariants in FocusList" testInvariantsInFocusList
    ]
