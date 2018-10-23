module Termonad.Prelude
  ( module X
  , whenJust
  ) where

import Control.Lens as X ((&))
import ClassyPrelude as X
import Data.Proxy as X

whenJust :: Monoid m => Maybe a -> (a -> m) -> m
whenJust = flip foldMap
