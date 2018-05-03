module Termonad.FocusedList where

import Termonad.Prelude

import Data.Finite (Finite)
import GHC.TypeLits (Nat)

data FocusedList :: Nat -> * -> * where
  FocusedList :: Finite n -> IntMap a -> FocusedList n a
