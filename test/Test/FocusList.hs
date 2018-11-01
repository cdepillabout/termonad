{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.FocusList where

import Termonad.Prelude

import Test.QuickCheck (Arbitrary, Gen, arbitrary)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (Spec, testSpec)
import Test.Validity (GenInvalid, GenUnchecked(genUnchecked, shrinkUnchecked), GenValid(genValid), Validity(validate), check, eqSpec, genValidSpec)

import Termonad.FocusList (Focus, FocusList, invariantFL)

import Test.FocusList.Invariants (testInvariantsInFocusList)

instance Arbitrary a => GenUnchecked (Seq a) where
  genUnchecked :: Gen (Seq a)
  genUnchecked = arbitrary

  shrinkUnchecked :: Seq a -> [Seq a]
  shrinkUnchecked = pure

instance GenUnchecked Focus

instance Arbitrary a => GenUnchecked (FocusList a)

instance Validity (FocusList a) where
  validate fl = check (invariantFL fl) "the FocusList has been constructed correctly"

instance Arbitrary a => GenValid (FocusList a) where
  genValid :: Gen (FocusList a)
  genValid = arbitrary

instance Arbitrary a => GenInvalid (FocusList a)

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
