{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.FocusList where

import Termonad.Prelude

import Data.GenValidity.Sequence ()
import Test.QuickCheck (Gen)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (Spec, testSpec)
import Test.Validity (GenInvalid, GenUnchecked, GenValid(genValid), Validity(validate), check, eqSpec, genValidSpec)

import Termonad.FocusList (Focus, FocusList, genValidFL, invariantFL)

import Test.FocusList.Invariants (testInvariantsInFocusList)

instance GenUnchecked Focus

instance GenUnchecked a => GenUnchecked (FocusList a)

instance Validity (FocusList a) where
  validate fl = check (invariantFL fl) "the FocusList has been constructed correctly"

instance GenValid a => GenValid (FocusList a) where
  genValid :: Gen (FocusList a)
  genValid = genValidFL genValid

instance GenInvalid a => GenInvalid (FocusList a)

focusListTestsIO :: IO TestTree
focusListTestsIO = do
  specs <- testSpec "validity tests" validitySpec
  pure $
    testGroup
      "FocusList"
      [ testProperty "invariants in FocusList" testInvariantsInFocusList
      , specs
      ]

validitySpec :: Spec
validitySpec = do
  eqSpec @(FocusList String)
  genValidSpec @(FocusList String)
